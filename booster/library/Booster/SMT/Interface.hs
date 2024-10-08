{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Booster.SMT.Interface (
    SMTContext, -- re-export
    SMTOptions (..), -- re-export
    defaultSMTOptions, -- re-export
    SMTError (..),
    UnknownReason (..),
    initSolver,
    noSolver,
    finaliseSolver,
    getModelFor,
    checkPredicates,
    hardResetSolver,
    pattern IsUnknown,
    IsSatResult,
    showIsSatResult,
    pattern IsSat,
    pattern IsUnsat,
    IsValidResult,
    pattern IsValid,
    pattern IsInvalid,
    isSat,
) where

import Control.Exception (Exception, throw)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Aeson (object, (.=))
import Data.Aeson.Types (FromJSON (..), ToJSON (..))
import Data.ByteString.Char8 qualified as BS
import Data.Coerce
import Data.Data (Proxy)
import Data.Either (isLeft)
import Data.Either.Extra (fromLeft', fromRight')
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text as Text (Text, pack, unlines, unwords)
import Deriving.Aeson (
    CamelToKebab,
    CustomJSON (..),
    FieldLabelModifier,
    OmitNothingFields,
    StripPrefix,
 )
import GHC.Generics (Generic)
import Prettyprinter (Pretty, backslash, hsep, punctuate, slash, (<+>))
import SMTLIB.Backends.Process qualified as Backend

import Booster.Definition.Base
import Booster.Log qualified as Log
import Booster.Pattern.Base
import Booster.Pattern.Pretty
import Booster.Pattern.Util (sortOfTerm)
import Booster.Prettyprinter qualified as Pretty
import Booster.SMT.Base as SMT
import Booster.SMT.Runner as SMT
import Booster.SMT.Translate as SMT
import Booster.Syntax.Json.Externalise (externaliseTerm)
import Booster.Util (Flag (..))

data SMTError
    = GeneralSMTError Text
    | SMTTranslationError Text
    deriving (Eq, Show)

instance Exception SMTError

throwSMT :: Text -> a
throwSMT = throw . GeneralSMTError

throwSMT' :: String -> a
throwSMT' = throwSMT . pack

smtTranslateError :: Text -> a
smtTranslateError = throw . SMTTranslationError

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

    evalSMT ctxt (runPrelude CheckSMTPrelude)
    Log.logMessage ("Successfully initialised SMT solver with " <> (Text.pack . show $ smtOptions))
    pure ctxt

{- | Returns an @SMTContext@ with no solver handle, essentially just a dummy that always returns `Unknown` for any command that is attempted.
This can be useful for unit testing or in case the user wants to call the booster without Z3.
-}
noSolver :: MonadIO io => io SMT.SMTContext
noSolver = do
    solverClose <- liftIO $ newIORef $ pure ()
    pure
        SMTContext
            { mbSolver = Nothing
            , solverClose
            , mbTranscriptHandle = Nothing
            , prelude = []
            , options = defaultSMTOptions{retryLimit = Just 0}
            }

-- | Stop the solver, initialise a new one, set the timeout and re-check the prelude
hardResetSolver :: Log.LoggerMIO io => SMT io ()
hardResetSolver = do
    ctxt <- SMT get
    case ctxt.mbSolver of
        Nothing -> pure ()
        Just solverRef -> do
            closeContext ctxt
            Log.logMessage ("Restarting SMT solver" :: Text)
            (solver, handle) <- connectToSolver ctxt.options.args
            liftIO $ do
                writeIORef solverRef solver
                writeIORef ctxt.solverClose $ Backend.close handle
            runPrelude NoCheckSMTPrelude

-- | Retry the action `cb`, first decreasing the retry counter and increasing the timeout limit, unless the retry limit has already been reached, in which case call `onTimeout`
retry :: Log.LoggerMIO io => SMT io a -> SMT io a -> SMT io a
retry cb onTimeout = do
    ctxt <- SMT get
    case ctxt.options.retryLimit of
        Just x | x > 0 -> do
            let timeout = 2 * ctxt.options.timeout
                retryLimit = ((-) 1) <$> ctxt.options.retryLimit
            Log.logMessage $ "Setting SMT retry limit to: " <> maybe "no retries" show retryLimit
            SMT $ put ctxt{options = ctxt.options{timeout, retryLimit}}
            hardResetSolver
            cb
        _ -> onTimeout

translatePrelude :: Log.LoggerMIO io => KoreDefinition -> io [DeclareCommand]
translatePrelude def =
    let prelude = smtDeclarations def
     in case prelude of
            Left err -> do
                Log.logMessage $ "Error translating definition to SMT: " <> err
                throwSMT $ "Unable to translate elements of the definition to SMT: " <> err
            Right decls -> pure decls

pattern CheckSMTPrelude, NoCheckSMTPrelude :: Flag "CheckSMTPrelude"
pattern CheckSMTPrelude = Flag True
pattern NoCheckSMTPrelude = Flag False

runPrelude :: Log.LoggerMIO io => Flag "CheckSMTPrelude" -> SMT io ()
runPrelude doCheck = do
    ctxt <- SMT get
    -- set the user defined timeout for queries
    setTimeout ctxt.options.timeout
    Log.logMessage ("Checking definition prelude" :: Text)
    -- send the commands from the definition's SMT prelude
    mapM_ runCmd ctxt.prelude
    -- optionally check the prelude for consistency
    when (coerce doCheck) $ do
        check <- runCmd CheckSat
        case check of
            Sat -> pure ()
            other -> do
                Log.logMessage $ "Initial SMT definition check returned " <> pack (show other)
                closeContext ctxt
                throwSMT' $
                    "Aborting due to potentially-inconsistent SMT setup: Initial check returned " <> show other
  where
    setTimeout timeout = do
        Log.logMessage $ "Setting SMT timeout to: " <> show timeout
        runCmd_ $ SetTimeout timeout

finaliseSolver :: Log.LoggerMIO io => SMT.SMTContext -> io ()
finaliseSolver ctxt = do
    Log.logMessage ("Closing SMT solver" :: Text)
    destroyContext ctxt

data UnknownReason
    = -- | SMT prelude is UNSAT
      InconsistentGroundTruth
    | -- | (P, not P) is (SAT, SAT)
      ImplicationIndeterminate
    | -- | SMT solver returned unknown
      SMTUnknownReason Text
    deriving (Show, Eq, Generic)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[OmitNothingFields, FieldLabelModifier '[CamelToKebab, StripPrefix "_"]] UnknownReason

instance Log.ToLogFormat UnknownReason where
    toTextualLog = pack . show
    toJSONLog = toJSON

pattern IsUnknown :: UnknownReason -> Either UnknownReason b
pattern IsUnknown u = Left u

newtype IsSat' a = IsSat' (Maybe a) deriving (Functor, Show, Eq)

type IsSatResult a = Either UnknownReason (IsSat' a)

pattern IsSat :: a -> IsSatResult a
pattern IsSat a = Right (IsSat' (Just a))

pattern IsUnsat :: IsSatResult a
pattern IsUnsat = Right (IsSat' Nothing)

showIsSatResult :: IsSatResult a -> Text
showIsSatResult = \case
    IsSat{} -> "SAT"
    IsUnsat -> "UNSAT"
    IsUnknown{} -> "UNKNOWN"

{-# COMPLETE IsSat, IsUnsat, IsUnknown #-}

{- | Check satisfiability of  predicates and substitutions.
     The set of input predicates @ps@ togehter with the substitutions @subst@ are interpreted as a conjunction.
-}
isSatReturnTransState ::
    forall io.
    Log.LoggerMIO io =>
    SMT.SMTContext ->
    [Predicate] ->
    Map Variable Term -> -- supplied substitution
    io (IsSatResult TranslationState)
isSatReturnTransState ctxt ps subst
    | null ps && Map.null subst = pure $ IsSat $ TranslationState{mappings = mempty, counter = 1}
    | Left errMsg <- translated = Log.withContext Log.CtxSMT $ do
        Log.withContext Log.CtxAbort $ Log.logMessage $ "SMT translation error: " <> errMsg
        smtTranslateError errMsg
    | Right (smtToCheck, transState) <- translated = Log.withContext Log.CtxSMT $ do
        evalSMT ctxt $
            hardResetSolver >> solve smtToCheck transState
  where
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

    solve smtToCheck transState = do
        Log.getPrettyModifiers >>= \case
            ModifiersRep (_ :: FromModifiersT mods => Proxy mods) ->
                Log.logMessage . Pretty.renderOneLineText $
                    hsep ("Predicates to check:" : map (pretty' @mods) ps)
        declareVariables transState
        mapM_ SMT.runCmd_ smtToCheck
        SMT.runCmd CheckSat >>= \case
            Sat -> pure $ IsSat transState
            Unsat -> pure IsUnsat
            Unknown reason -> retry (solve smtToCheck transState) (pure $ IsUnknown (SMTUnknownReason reason))
            other -> do
                let msg = "Unexpected result while calling 'check-sat': " <> show other
                Log.withContext Log.CtxAbort $ Log.logMessage $ Text.pack msg
                throwSMT' msg

isSat ::
    forall io.
    Log.LoggerMIO io =>
    SMT.SMTContext ->
    [Predicate] ->
    io (IsSatResult ())
isSat ctxt ps = fmap void <$> (isSatReturnTransState ctxt ps mempty)

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
    io (IsSatResult (Map Variable Term))
getModelFor ctxt ps subst
    | null ps && Map.null subst = Log.withContext Log.CtxSMT $ do
        Log.logMessage ("No constraints or substitutions to check, returning Sat" :: Text)
        pure $ IsSat Map.empty
    | otherwise = Log.withContext Log.CtxSMT $ do
        evalSMT ctxt $
            hardResetSolver >> isSatReturnTransState ctxt ps subst >>= \case
                IsSat transState -> IsSat <$> extractModel transState
                IsUnsat -> pure IsUnsat
                IsUnknown reason -> pure $ IsUnknown reason
  where
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
            Log.getPrettyModifiers >>= \case
                ModifiersRep (_ :: FromModifiersT mods => Proxy mods) ->
                    let vars = Pretty.renderText . hsep . map (pretty' @mods) $ Map.keys untranslatableVars
                     in Log.logMessage ("Untranslatable variables in model: " <> vars)

        response <-
            if Map.null freeVarsMap
                then pure $ Values []
                else SMT.runCmd $ GetValue (Map.elems freeVarsMap)
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

mkComment :: Pretty (PrettyWithModifiers '[Decoded] a) => a -> BS.ByteString
mkComment = BS.pack . Pretty.renderDefault . pretty' @'[Decoded]

newtype IsValid' = IsValid' Bool

type IsValidResult = Either UnknownReason IsValid'

pattern IsValid, IsInvalid :: IsValidResult
pattern IsValid = Right (IsValid' True)
pattern IsInvalid = Right (IsValid' False)

{-# COMPLETE IsValid, IsInvalid, IsUnknown #-}

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
    io IsValidResult
checkPredicates ctxt givenPs givenSubst psToCheck
    | null psToCheck = pure IsValid
    | Left errMsg <- translated = Log.withContext Log.CtxSMT $ do
        Log.withContext Log.CtxAbort $ Log.logMessage $ "SMT translation error: " <> errMsg
        smtTranslateError errMsg
    | Right ((smtGiven, sexprsToCheck), transState) <- translated = Log.withContext Log.CtxSMT $ do
        evalSMT ctxt $
            hardResetSolver >> solve smtGiven sexprsToCheck transState
  where
    solve ::
        [DeclareCommand] ->
        [SExpr] ->
        TranslationState ->
        SMT io IsValidResult
    solve smtGiven sexprsToCheck transState = do
        declareVariables transState
        Log.logMessage $
            Text.unwords
                [ "Checking"
                , pack (show $ length psToCheck)
                , "predicates, given"
                , pack (show $ length givenPs)
                , "assertions and a substitution of size"
                , pack (show $ Map.size givenSubst)
                ]
        Log.getPrettyModifiers >>= \case
            ModifiersRep (_ :: FromModifiersT mods => Proxy mods) ->
                Log.logMessage . Pretty.renderOneLineText $
                    hsep ("Predicates to check:" : map (pretty' @mods) (Set.toList psToCheck))
        result <- interactWithSolver smtGiven sexprsToCheck
        case result of
            (Unsat, Unsat) -> pure $ IsUnknown InconsistentGroundTruth
            (Sat, Sat) -> do
                Log.logMessage ("Implication not determined" :: Text)
                pure $ IsUnknown ImplicationIndeterminate
            (Sat, Unsat) -> pure IsValid
            (Unsat, Sat) -> pure IsInvalid
            (Unknown reason, _) -> retry (solve smtGiven sexprsToCheck transState) (pure . IsUnknown . SMTUnknownReason $ reason)
            (_, Unknown reason) -> retry (solve smtGiven sexprsToCheck transState) (pure . IsUnknown . SMTUnknownReason $ reason)
            other ->
                throwSMT $
                    "Unexpected result while checking a condition: " <> Text.pack (show other)

    translated :: Either Text (([DeclareCommand], [SExpr]), TranslationState)
    translated = SMT.runTranslator $ do
        let mkSMTEquation v t =
                SMT.eq <$> SMT.translateTerm (Var v) <*> SMT.translateTerm t
        substEqs <-
            mapM (uncurry mkSMTEquation) $ Map.assocs givenSubst

        groundTruth <-
            mapM (\(Predicate p) -> (p,) <$> SMT.translateTerm p) $ Set.toList givenPs

        toCheck <-
            mapM (SMT.translateTerm . coerce) $ Set.toList psToCheck

        let interestingVars = mconcat $ map smtVars toCheck
            filteredGroundTruth = closureOver interestingVars $ Set.fromList $ map snd groundTruth
            filteredSubstEqs = closureOver interestingVars $ Set.fromList substEqs

        let mkAssert (p, sexpr) = Assert (mkComment p) sexpr
            smtPs = map mkAssert $ filter ((`Set.member` filteredGroundTruth) . snd) groundTruth
            smtSubst = map (Assert "Substitution") $ Set.toList filteredSubstEqs

        pure (smtSubst <> smtPs, toCheck)

    -- Given the known truth and the expressions to check,
    -- interact with the solver to establish the validity of the  expressions.
    --
    -- the solver effects are localised to this function:
    -- - pushing and popping of the assertion context
    interactWithSolver ::
        [DeclareCommand] -> [SExpr] -> SMT io (Response, Response)
    interactWithSolver smtGiven sexprsToCheck = do
        SMT.runCmd_ Push

        -- assert ground truth
        mapM_ SMT.runCmd_ smtGiven

        groundTruthCheckSmtResult <- SMT.runCmd CheckSat
        Log.logMessage ("Ground truth check returned: " <> show groundTruthCheckSmtResult)
        case groundTruthCheckSmtResult of
            Unsat -> do
                Log.logMessage ("Inconsistent ground truth" :: Text)
                pure (Unsat, Unsat)
            Unknown reason -> do
                Log.getPrettyModifiers >>= \case
                    ModifiersRep (_ :: FromModifiersT mods => Proxy mods) -> do
                        Log.withContext Log.CtxDetail
                            $ Log.logMessage
                            $ Log.WithJsonMessage
                                (object ["conditions" .= (map (externaliseTerm . coerce) . Set.toList $ givenPs)])
                            $ Pretty.renderOneLineText
                            $ "Unknown ground truth: "
                                <+> (hsep . punctuate (slash <> backslash) . map (pretty' @mods) . Set.toList $ givenPs)
                pure (Unknown reason, Unknown reason)
            _ -> do
                -- save ground truth for 2nd check
                SMT.runCmd_ Push

                -- run check for K ∧ P and then for K ∧ !P
                let allToCheck = SMT.List (Atom "and" : sexprsToCheck)

                positive <- do
                    SMT.runCmd_ $ Assert "P" allToCheck
                    SMT.runCmd CheckSat
                SMT.runCmd_ Pop

                negative <- do
                    SMT.runCmd_ $ Assert "not P" (SMT.smtnot allToCheck)
                    SMT.runCmd CheckSat
                SMT.runCmd_ Pop

                Log.logMessage $
                    "Check of Given ∧ P and Given ∧ !P produced "
                        <> pack (show (positive, negative))

                let (positive', negative') =
                        case (positive, negative) of
                            (Unsat, _) -> (Unsat, Sat)
                            (_, Unsat) -> (Sat, Unsat)
                            _ -> (positive, negative)
                unless ((positive, negative) == (positive', negative')) $
                    Log.logMessage $
                        "Given ∧ P and Given ∧ !P interpreted as "
                            <> pack (show (positive', negative'))
                pure (positive', negative')

    -- functions for filtering ground truth and substitution equations
    smtVars :: SMT.SExpr -> Set SMT.SMTId
    smtVars (Atom smtId@(SMTId bs))
        | "SMT-" `BS.isPrefixOf` bs = Set.singleton smtId
        | otherwise = mempty
    smtVars (List exprs) = mconcat $ map smtVars exprs

    -- filters given 'exprs' to only return those which use any of the
    -- SMT 'atoms' or from other expressions that are also returned.
    closureOver :: Set SMT.SMTId -> Set SMT.SExpr -> Set SMT.SExpr
    closureOver atoms exprs = loop mempty exprs atoms
      where
        loop :: Set SMT.SExpr -> Set SMT.SExpr -> Set SMT.SMTId -> Set SMT.SExpr
        loop acc exprs' currentAtoms =
            let (rest, addedExprs) = Set.partition (Set.null . Set.intersection currentAtoms . smtVars) exprs'
                newAtoms = Set.unions $ Set.map smtVars addedExprs
             in if Set.null addedExprs
                    then acc
                    else loop (acc <> addedExprs) rest newAtoms
