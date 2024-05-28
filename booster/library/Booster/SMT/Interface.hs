{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Interface (
    SMTContext, -- re-export
    SMTError (..),
    SMTOptions (..),
    defaultSMTOptions,
    initSolver,
    finaliseSolver,
    getModelFor,
    checkPredicates,
) where

import Control.Exception (Exception, throw)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.ByteString.Char8 qualified as BS
import Data.Coerce
import Data.Either (isLeft)
import Data.Either.Extra (fromLeft', fromRight')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text as Text (Text, pack, unlines, unwords)
import Prettyprinter (Pretty, hsep, pretty)
import SMTLIB.Backends.Process qualified as Backend

import Booster.Definition.Base
import Booster.Log qualified as Log
import Booster.Pattern.Base
import Booster.Pattern.Util (sortOfTerm)
import Booster.Prettyprinter qualified as Pretty
import Booster.SMT.Base as SMT
import Booster.SMT.Runner as SMT
import Booster.SMT.Translate as SMT
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Logger qualified as Log

-- Includes all options from kore-rpc used by current clients. The
-- parser in CLOptions uses compatible names and we use the same
-- defaults. Not all options are supported in booster.
data SMTOptions = SMTOptions
    { transcript :: Maybe FilePath
    -- ^ optional log file
    , timeout :: Int
    -- ^ optional timeout for requests, 0 for none
    , retryLimit :: Maybe Int
    -- ^ optional retry. Nothing for no retry, 0 for unlimited
    , tactic :: Maybe SExpr
    -- ^ optional tactic (used verbatim) to replace (check-sat)
    }
    deriving (Eq, Show)

data SMTError
    = GeneralSMTError Text
    | SMTTranslationError Text
    | SMTSolverUnknown Text (Set Predicate) (Set Predicate)
    deriving (Eq, Show)

instance Exception SMTError

throwSMT :: Text -> a
throwSMT = throw . GeneralSMTError

throwSMT' :: String -> a
throwSMT' = throwSMT . pack

throwUnknown :: Text -> Set Predicate -> Set Predicate -> a
throwUnknown reason premises preds = throw $ SMTSolverUnknown reason premises preds

smtTranslateError :: Text -> a
smtTranslateError = throw . SMTTranslationError

defaultSMTOptions :: SMTOptions
defaultSMTOptions =
    SMTOptions
        { transcript = Nothing
        , timeout = 125
        , retryLimit = Just 3
        , tactic = Nothing
        }

{- | Start and initialise an SMT solver instance for use in rewriting:
     - translate the sort declarations from @KoreDefiniton@ to SMT
     - start the solver process
     - feed in the prelude and check it for consistency
     - set user-specified timeout for queries
-}
initSolver :: Log.LoggerMIO io => KoreDefinition -> SMTOptions -> io SMT.SMTContext
initSolver def smtOptions = do
    prelude <- translatePrelude def

    Log.logMessage ("Starting new SMT solver" :: Text)
    ctxt <- mkContext prelude smtOptions.transcript

    evalSMT ctxt $ do
        checkPrelude
        -- set timeout value for the general queries
        runCmd_ $ SetTimeout smtOptions.timeout
    Log.logMessage ("Successfully initialised SMT solver with " <> (Text.pack . show $ smtOptions))
    pure ctxt

restartSolver :: forall io. Log.LoggerMIO io => SMTOptions -> SMT io ()
restartSolver smtOptions = do
    Log.logMessage ("Starting new SMT solver" :: Text)
    ctxt <- SMT get
    liftIO ctxt.solverClose
    (solver, handle) <- connectToSolver
    SMT $ put ctxt{solver, solverClose = Backend.close handle}

    checkPrelude
    Log.logMessage ("Successfully initialised SMT solver with " <> (Text.pack . show $ smtOptions))

    -- set timeout value for the general queries
    runCmd_ $ SetTimeout smtOptions.timeout

translatePrelude :: Log.LoggerMIO io => KoreDefinition -> io [DeclareCommand]
translatePrelude def =
    let prelude = smtDeclarations def
     in case prelude of
            Left err -> do
                Log.logMessage $ "Error translating definition to SMT: " <> err
                throwSMT $ "Unable to translate elements of the definition to SMT: " <> err
            Right decls -> pure decls

checkPrelude :: Log.LoggerMIO io => SMT io ()
checkPrelude = do
    runCmd_ $ SetTimeout defaultSMTOptions.timeout
    Log.logMessage ("Checking definition prelude" :: Text)
    check <- runPrelude >> runCmd CheckSat
    case check of
        Sat -> pure ()
        other -> do
            Log.logMessage $ "Initial SMT definition check returned " <> pack (show other)
            SMT get >>= closeContext
            throwSMT' $
                "Aborting due to potentially-inconsistent SMT setup: Initial check returned " <> show other

-- | Send the commands from the definition's SMT prelude
runPrelude :: Log.LoggerMIO io => SMT io ()
runPrelude = do
    prelude <- SMT $ gets prelude
    mapM_ runCmd prelude

finaliseSolver :: Log.LoggerMIO io => SMT.SMTContext -> io ()
finaliseSolver ctxt = do
    Log.logMessage ("Closing SMT solver" :: Text)
    closeContext ctxt

{- |
Implementation of get-model request

Queries an SMT solver (given by SMTContext but assumed uninitialised,
passing the definition) for whether the given predicates and
the supplied substitution are satisfiable together.

Returns a satisfying substitution of free variables
in the predicates if so.

Returns either 'Unsat' or 'Unknown' otherwise, depending on whether
the solver could determine 'Unsat'.
-}
getModelFor ::
    forall io.
    Log.LoggerMIO io =>
    MonadLoggerIO io =>
    SMTOptions ->
    KoreDefinition ->
    [Predicate] ->
    Map Variable Term -> -- supplied substitution
    io (Either SMT.Response (Map Variable Term))
getModelFor smtOptions def ps subst
    | null ps && Map.null subst = Log.withContext "smt" $ do
        Log.logMessage ("No constraints or substitutions to check, returning Sat" :: Text)
        pure $ Right Map.empty
    | Left errMsg <- translated = Log.withContext "smt" $ do
        Log.logErrorNS "booster" $ "SMT translation error: " <> errMsg
        Log.logMessage $ "SMT translation error: " <> errMsg
        smtTranslateError errMsg
    | Right (smtAsserts, transState) <- translated = Log.withContext "smt" $ do
        ctxt <- initSolver def smtOptions
        (result, newCtxt) <- runSMT ctxt $ do
            solve smtOptions smtAsserts transState
        finaliseSolver newCtxt
        pure result
  where
    solve ::
        SMTOptions -> [DeclareCommand] -> TranslationState -> SMT io (Either Response (Map Variable Term))
    solve opts smtAsserts transState = do
        Log.logMessage $ "Checking, constraint count " <> pack (show $ Map.size subst + length ps)
        satResponse <- interactWithSolver smtAsserts transState
        Log.logMessage ("Solver returned " <> (Text.pack $ show satResponse))
        case satResponse of
            Error msg -> do
                runCmd_ SMT.Pop
                throwSMT' $ BS.unpack msg
            Unsat -> do
                runCmd_ SMT.Pop
                pure $ Left Unsat
            Unknown{} -> do
                case opts.retryLimit of
                    Just x | x > 0 -> do
                        let newOpts = opts{timeout = 2 * opts.timeout, retryLimit = Just $ x - 1}
                        restartSolver newOpts
                        solve newOpts smtAsserts transState
                    _ -> getReasonUnknown
            r@ReasonUnknown{} ->
                pure $ Left r
            Values{} -> do
                runCmd_ SMT.Pop
                throwSMT' $ "Unexpected SMT response to CheckSat: " <> show satResponse
            Success -> do
                runCmd_ SMT.Pop
                throwSMT' $ "Unexpected SMT response to CheckSat: " <> show satResponse
            Sat -> Right <$> extractModel transState

    getReasonUnknown :: SMT io (Either Response (Map Variable Term))
    getReasonUnknown = do
        res <- runCmd SMT.GetReasonUnknown
        runCmd_ SMT.Pop
        pure . Left $ res

    translated :: Either Text ([DeclareCommand], TranslationState)
    translated =
        SMT.runTranslator $ do
            let mkSMTEquation v t =
                    SMT.eq <$> SMT.translateTerm (Var v) <*> SMT.translateTerm t
            smtSubst <-
                mapM (\(v, t) -> Assert "Substitution" <$> mkSMTEquation v t) $ Map.assocs subst
            smtPs <-
                mapM (\(Predicate p) -> Assert (mkComment p) <$> SMT.translateTerm p) ps
            pure $ smtSubst <> smtPs

    interactWithSolver :: [DeclareCommand] -> TranslationState -> SMT io Response
    interactWithSolver smtAsserts transState = do
        runCmd_ SMT.Push -- assuming the prelude has been run already,

        -- declare-const all introduced variables (free in predicates
        -- as well as abstraction variables) before sending assertions
        mapM_
            runCmd
            [ DeclareConst (mkComment trm) smtId (SMT.smtSort $ sortOfTerm trm)
            | (trm, smtId) <- Map.assocs transState.mappings
            ]

        -- assert the given predicates
        mapM_ runCmd smtAsserts

        runCmd CheckSat

    extractModel ::
        TranslationState ->
        SMT io (Map Variable Term)
    extractModel transState = do
        Log.logMessage ("Extracting model" :: Text)
        let freeVars =
                Set.unions $
                    Map.keysSet subst : map ((.variables) . getAttributes . coerce) ps

            freeVarsMap =
                Map.map Atom . Map.mapKeys getVar $
                    Map.filterWithKey
                        (const . (`Set.member` Set.map Var freeVars))
                        transState.mappings
            getVar (Var v) = v
            getVar other =
                smtTranslateError . pack $
                    "Solver returned non-var in translation state: " <> show other
            sortsToTranslate = Set.fromList [SortInt, SortBool]

            (freeVarsToSExprs, untranslatableVars) =
                Map.partitionWithKey
                    (const . ((`Set.member` sortsToTranslate) . (.variableSort)))
                    freeVarsMap
        unless (Map.null untranslatableVars) $
            let vars = Pretty.renderText . hsep . map pretty $ Map.keys untranslatableVars
             in Log.logMessage ("Untranslatable variables in model: " <> vars)

        response <-
            if Map.null freeVarsMap
                then pure $ Values []
                else runCmd $ GetValue (Map.elems freeVarsMap)
        runCmd_ SMT.Pop
        case response of
            Error msg ->
                throwSMT' $ BS.unpack msg
            Values pairs ->
                let (errors, values) =
                        Map.partition isLeft
                            . Map.map (valueToTerm transState)
                            $ Map.compose (Map.fromList pairs) freeVarsToSExprs
                    untranslated =
                        Map.mapWithKey (const . Var) untranslatableVars
                 in if null errors
                        then pure $ Map.map fromRight' values <> untranslated
                        else
                            throwSMT . Text.unlines $
                                ( "SMT errors while converting results: "
                                    : map fromLeft' (Map.elems errors)
                                )
            other ->
                throwSMT' $ "Unexpected SMT response to GetValue: " <> show other

mkComment :: Pretty a => a -> BS.ByteString
mkComment = BS.pack . Pretty.renderDefault . pretty

{- | Check a predicates, given a set of predicates as known truth.

Simplest version:

Given K as known truth and predicates P to check, check whether K => P
or K => !P, or neither of those implications holds. The check is done
by asking the solver to find a counter-example:

- If K ∧ !P yields Unsat then K => P (P is true given K)
- If K ∧ P yields Unsat then K => !P (P is false given K)

For both cases, the respective other result should be Sat, not
Unknown, but we can _assume_ that by excluded middle (P ∨ !P).

- If both K ∧ !P and K ∧ P yield Unsat then K is already Unsat (i.e.,
  false) by itself and we should not conclude anything.
- All other cases imply that we cannot conclude any impliciation,
  neither P nor !P follows from K.

In the initial version we make no attempt to determine _which_ of the
predicates in P contributes to the respective false result. Neither do
we check whether the predicates in P are satisfiable at all by
themselves (together, without constraints in K).
-}
checkPredicates ::
    forall io.
    Log.LoggerMIO io =>
    MonadLoggerIO io =>
    SMT.SMTContext ->
    Set Predicate ->
    Map Variable Term ->
    Set Predicate ->
    io (Maybe Bool)
checkPredicates ctxt givenPs givenSubst psToCheck
    | null psToCheck = pure $ Just True -- or Nothing?
    | Left errMsg <- translated = Log.withContext "smt" $ do
        Log.logErrorNS "booster" $ "SMT translation error: " <> errMsg
        Log.logMessage $ "SMT translation error: " <> errMsg
        pure Nothing
    | Right ((smtGiven, sexprsToCheck), transState) <- translated = Log.withContext "smt" $ SMT.evalSMT ctxt . runMaybeT $ do
        Log.logMessage $
            Text.unwords
                [ "Checking"
                , pack (show $ length psToCheck)
                , "predicates, given"
                , pack (show $ length givenPs)
                , "assertions and a substitution of size"
                , pack (show $ Map.size givenSubst)
                ]
        Log.logMessage . Pretty.renderOneLineText $
            hsep ("Predicates to check:" : map pretty (Set.toList psToCheck))

        (positive, negative) <- interactWihtSolver smtGiven sexprsToCheck transState

        Log.logMessage $
            "Check of Given ∧ P and Given ∧ !P produced "
                <> pack (show (positive, negative))

        processSMTResult positive negative (retryOnce smtGiven sexprsToCheck transState)
  where
    processSMTResult :: Response -> Response -> MaybeT (SMT io) Bool -> MaybeT (SMT io) Bool
    processSMTResult positive negative onUnknown =
        case (positive, negative) of
            (Unsat, Unsat) -> throwSMT "Inconsistent ground truth: should have been caught above"
            (Sat, Sat) -> fail "Implication not determined"
            (Sat, Unsat) -> pure True
            (Unsat, Sat) -> pure False
            (Unknown, _) -> onUnknown
            (_, Unknown) -> onUnknown
            other -> throwSMT' $ "Unexpected result while checking a condition: " <> show other

    retryOnce :: [DeclareCommand] -> [SExpr] -> TranslationState -> MaybeT (SMT io) Bool
    retryOnce smtGiven sexprsToCheck transState = do
        lift (restartSolver defaultSMTOptions)
        (positive, negative) <- interactWihtSolver smtGiven sexprsToCheck transState
        processSMTResult positive negative failBecauseUnknown

    smtRun_ :: SMTEncode c => c -> MaybeT (SMT io) ()
    smtRun_ = lift . SMT.runCmd_
    smtRun :: SMTEncode c => c -> MaybeT (SMT io) Response
    smtRun = lift . SMT.runCmd

    translated :: Either Text (([DeclareCommand], [SExpr]), TranslationState)
    translated = SMT.runTranslator $ do
        let mkSMTEquation v t =
                SMT.eq <$> SMT.translateTerm (Var v) <*> SMT.translateTerm t
        smtSubst <-
            mapM (\(v, t) -> Assert "Substitution" <$> mkSMTEquation v t) $ Map.assocs givenSubst
        smtPs <-
            mapM (\(Predicate p) -> Assert (mkComment p) <$> SMT.translateTerm p) $ Set.toList givenPs
        toCheck <-
            mapM (SMT.translateTerm . coerce) $ Set.toList psToCheck
        pure (smtSubst <> smtPs, toCheck)

    failBecauseUnknown :: MaybeT (SMT io) Bool
    failBecauseUnknown =
        smtRun GetReasonUnknown >>= \case
            ReasonUnknown reason -> throwUnknown reason givenPs psToCheck
            other -> throwSMT' $ "Unexpected result while calling ':reason-unknown': " <> show other

    interactWihtSolver ::
        [DeclareCommand] -> [SExpr] -> TranslationState -> MaybeT (SMT io) (Response, Response)
    interactWihtSolver smtGiven sexprsToCheck transState = do
        smtRun_ Push

        -- declare-const all introduced variables (free in predicates
        -- as well as abstraction variables) before sending assertions
        mapM_
            smtRun
            [ DeclareConst (mkComment trm) smtId (SMT.smtSort $ sortOfTerm trm)
            | (trm, smtId) <- Map.assocs transState.mappings
            ]

        -- assert ground truth
        mapM_ smtRun smtGiven

        consistent <- smtRun CheckSat
        when (consistent /= Sat) $ do
            void $ smtRun Pop
            Log.logMessage ("Inconsistent ground truth, check returns Nothing" :: Text)
            fail "returns nothing"

        -- save ground truth for 2nd check
        smtRun_ Push

        -- run check for K ∧ P and then for K ∧ !P
        let allToCheck = SMT.List (Atom "and" : sexprsToCheck)

        smtRun_ $ Assert "P" allToCheck
        positive <- smtRun CheckSat
        smtRun_ Pop
        smtRun_ $ Assert "not P" (SMT.smtnot allToCheck)
        negative <- smtRun CheckSat
        void $ smtRun Pop

        pure (positive, negative)
