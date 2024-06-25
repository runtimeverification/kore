{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Interface (
    SMTContext, -- re-export
    SMTOptions (..), -- re-export
    defaultSMTOptions, -- re-export
    SMTError (..),
    initSolver,
    finaliseSolver,
    getModelFor,
    checkPredicates,
    hardResetSolver,
) where

import Control.Exception (Exception, throw)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
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

smtTranslateError :: Text -> a
smtTranslateError = throw . SMTTranslationError

smtRun_ :: Log.LoggerMIO io => SMTEncode c => c -> ExceptT SMTError (SMT io) ()
smtRun_ = lift . SMT.runCmd_

smtRun :: Log.LoggerMIO io => SMTEncode c => c -> ExceptT SMTError (SMT io) Response
smtRun = lift . SMT.runCmd

{- | declare-const all introduced variables (free in predicates
  as well as abstraction variables) before sending assertions
-}
declareVariables :: Log.LoggerMIO io => TranslationState -> SMT io ()
declareVariables transState = do
    mapM_
        SMT.runCmd
        [ DeclareConst (mkComment trm) smtId (SMT.smtSort $ sortOfTerm trm)
        | (trm, smtId) <- Map.assocs transState.mappings
        ]

{- | Start and initialise an SMT solver instance for use in rewriting:
     - translate the sort declarations from @KoreDefiniton@ to SMT
     - start the solver process
     - feed in the prelude and check it for consistency
     - set user-specified timeout for queries
-}
initSolver :: Log.LoggerMIO io => KoreDefinition -> SMTOptions -> io SMT.SMTContext
initSolver def smtOptions = Log.withContext Log.CtxSMT $ do
    prelude <- translatePrelude def

    Log.logMessage ("Starting new SMT solver" :: Text)
    ctxt <- mkContext smtOptions prelude

    evalSMT ctxt $ do
        checkPrelude
        -- set timeout value for the general queries
        runCmd_ $ SetTimeout smtOptions.timeout
    Log.logMessage ("Successfully initialised SMT solver with " <> (Text.pack . show $ smtOptions))
    pure ctxt

-- | Hot-swap @SMTOptions@ in the active @SMTContext@, update the query timeout
swapSmtOptions :: forall io. Log.LoggerMIO io => SMTOptions -> SMT io ()
swapSmtOptions smtOptions = do
    ctxt <- SMT get
    Log.logMessage ("Updating solver options with " <> (Text.pack . show $ smtOptions))
    SMT $ put ctxt{options = smtOptions}
    runCmd_ $ SetTimeout smtOptions.timeout

-- | Stop the solver, initialise a new one and put in the @SMTContext@
hardResetSolver :: forall io. Log.LoggerMIO io => SMTOptions -> SMT io ()
hardResetSolver smtOptions = do
    Log.logMessage ("Starting new SMT solver" :: Text)
    ctxt <- SMT get
    closeContext ctxt
    (solver, handle) <- connectToSolver
    SMT $ put ctxt{solver, solverClose = Backend.close handle}
    checkPrelude
    swapSmtOptions smtOptions

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
            SMT get >>= destroyContext
            throwSMT' $
                "Aborting due to potentially-inconsistent SMT setup: Initial check returned " <> show other

-- | Send the commands from the definition's SMT prelude
runPrelude :: Log.LoggerMIO io => SMT io ()
runPrelude = do
    prelude <- SMT $ gets prelude
    mapM_ runCmd prelude

finaliseSolver :: Log.LoggerMIO io => SMT.SMTContext -> io ()
finaliseSolver ctxt = Log.withContext Log.CtxSMT $ do
    Log.logMessage ("Closing SMT solver" :: Text)
    destroyContext ctxt

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
    SMT.SMTContext ->
    [Predicate] ->
    Map Variable Term -> -- supplied substitution
    io (Either SMTError (Either SMT.Response (Map Variable Term)))
getModelFor ctxt ps subst
    | null ps && Map.null subst = Log.withContext Log.CtxSMT $ do
        Log.logMessage ("No constraints or substitutions to check, returning Sat" :: Text)
        pure . Right . Right $ Map.empty
    | Left errMsg <- translated = Log.withContext Log.CtxSMT $ do
        Log.logMessage $ "SMT translation error: " <> errMsg
        smtTranslateError errMsg
    | Right (smtAsserts, transState) <- translated = Log.withContext Log.CtxSMT $ do
        evalSMT ctxt . runExceptT $ do
            lift $ hardResetSolver ctxt.options
            solve smtAsserts transState
  where
    solve ::
        [DeclareCommand] ->
        TranslationState ->
        ExceptT SMTError (SMT io) (Either Response (Map Variable Term))
    solve smtAsserts transState = do
        lift $ declareVariables transState
        opts <- lift . SMT $ gets (.options)
        Log.logMessage $ "Checking, constraint count " <> pack (show $ Map.size subst + length ps)
        interactWithSolver transState smtAsserts >>= \case
            Left response ->
                case response of
                    -- note that 'Unknown' will never be returned by 'interactWithSolver', as it will always be
                    -- converted to 'ReasonUnknown{}'.
                    ReasonUnknown{} -> do
                        case opts.retryLimit of
                            Just x | x > 0 -> do
                                let newOpts = opts{timeout = 2 * opts.timeout, retryLimit = Just $ x - 1}
                                lift $ hardResetSolver newOpts
                                solve smtAsserts transState
                            _ -> pure . Left $ response
                    _ -> pure . Left $ response
            Right model -> pure . Right $ model

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

    interactWithSolver ::
        TranslationState ->
        [DeclareCommand] ->
        ExceptT SMTError (SMT io) (Either Response (Map Variable Term))
    interactWithSolver transState smtAsserts = do
        smtRun_ SMT.Push -- assuming the prelude has been run already,

        -- assert the given predicates
        mapM_ smtRun smtAsserts

        satResponse <- smtRun CheckSat
        Log.logMessage ("Solver returned " <> (Text.pack $ show satResponse))

        result <- case satResponse of
            Error msg -> throwSMT' $ BS.unpack msg
            Unsat -> pure $ Left Unsat
            Unknown{} -> do
                -- we always request the reason, even if we later retry,
                -- to avoid delayed popping from the assertion stack in 'solve'
                reasonUnknown <- smtRun SMT.GetReasonUnknown
                pure $ Left reasonUnknown
            r@ReasonUnknown{} -> pure $ Left r
            Values{} -> throwSMT' $ "Unexpected SMT response to CheckSat: " <> show satResponse
            Success -> throwSMT' $ "Unexpected SMT response to CheckSat: " <> show satResponse
            Sat -> Right <$> extractModel transState
        smtRun_ SMT.Pop
        pure result

    extractModel ::
        TranslationState ->
        ExceptT SMTError (SMT io) (Map Variable Term)
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
                else smtRun $ GetValue (Map.elems freeVarsMap)
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
    SMT.SMTContext ->
    Set Predicate ->
    Map Variable Term ->
    Set Predicate ->
    io (Either SMTError (Maybe Bool))
checkPredicates ctxt givenPs givenSubst psToCheck
    | null psToCheck = pure . Right $ Just True
    | Left errMsg <- translated = Log.withContext Log.CtxSMT $ do
        Log.withContext Log.CtxAbort $ Log.logMessage $ "SMT translation error: " <> errMsg
        pure . Left . SMTTranslationError $ errMsg
    | Right ((smtGiven, sexprsToCheck), transState) <- translated = Log.withContext Log.CtxSMT $ do
        evalSMT ctxt . runExceptT $ do
            lift $ hardResetSolver ctxt.options
            solve smtGiven sexprsToCheck transState
  where
    solve ::
        [DeclareCommand] ->
        [SExpr] ->
        TranslationState ->
        ExceptT SMTError (SMT io) (Maybe Bool)
    solve smtGiven sexprsToCheck transState = do
        lift $ declareVariables transState
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
        result <- interactWithSolver smtGiven sexprsToCheck
        Log.logMessage $
            "Check of Given ∧ P and Given ∧ !P produced "
                <> (Text.pack $ show result)

        case result of
            (Unsat, Unsat) -> pure Nothing -- defensive choice for inconsistent ground truth
            (Sat, Sat) -> do
                Log.logMessage ("Implication not determined" :: Text)
                pure Nothing
            (Sat, Unsat) -> pure . Just $ True
            (Unsat, Sat) -> pure . Just $ False
            (Unknown, _) -> retry smtGiven sexprsToCheck transState
            (_, Unknown) -> retry smtGiven sexprsToCheck transState
            other ->
                throwE . GeneralSMTError $
                    ("Unexpected result while checking a condition: " :: Text) <> Text.pack (show other)

    retry :: [DeclareCommand] -> [SExpr] -> TranslationState -> ExceptT SMTError (SMT io) (Maybe Bool)
    retry smtGiven sexprsToCheck transState = do
        opts <- lift . SMT $ gets (.options)
        case opts.retryLimit of
            Just x | x > 0 -> do
                let newOpts = opts{timeout = 2 * opts.timeout, retryLimit = Just $ x - 1}
                lift $ hardResetSolver newOpts
                solve smtGiven sexprsToCheck transState
            _ -> failBecauseUnknown

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

    failBecauseUnknown :: ExceptT SMTError (SMT io) (Maybe Bool)
    failBecauseUnknown =
        smtRun GetReasonUnknown >>= \case
            ReasonUnknown reason -> do
                Log.withContext Log.CtxAbort $
                    Log.logMessage $
                        "Returned Unknown. Reason: " <> reason
                throwE $ SMTSolverUnknown reason givenPs psToCheck
            other -> do
                let msg = "Unexpected result while calling ':reason-unknown': " <> show other
                Log.withContext Log.CtxAbort $ Log.logMessage $ Text.pack msg
                throwSMT' msg

    -- Given the known truth and the expressions to check,
    -- interact with the solver to establish the validity of the  expressions.
    --
    -- the solver effects are localised to this function:
    -- - pushing and popping of the assertion context
    interactWithSolver ::
        [DeclareCommand] -> [SExpr] -> ExceptT SMTError (SMT io) (Response, Response)
    interactWithSolver smtGiven sexprsToCheck = do
        smtRun_ Push

        -- assert ground truth
        mapM_ smtRun smtGiven

        consistent <- smtRun CheckSat
        unless (consistent == Sat) $ do
            let errMsg = ("Inconsistent ground truth, check returns Nothing" :: Text)
            Log.logMessage errMsg
        let ifConsistent check = if (consistent == Sat) then check else pure Unsat

        -- save ground truth for 2nd check
        smtRun_ Push

        -- run check for K ∧ P and then for K ∧ !P
        let allToCheck = SMT.List (Atom "and" : sexprsToCheck)

        positive <- ifConsistent $ do
            smtRun_ $ Assert "P" allToCheck
            smtRun CheckSat
        smtRun_ Pop
        negative <- ifConsistent $ do
            smtRun_ $ Assert "not P" (SMT.smtnot allToCheck)
            smtRun CheckSat
        smtRun_ Pop

        Log.logMessage $
            "Check of Given ∧ P and Given ∧ !P produced "
                <> pack (show (positive, negative))

        pure (positive, negative)
