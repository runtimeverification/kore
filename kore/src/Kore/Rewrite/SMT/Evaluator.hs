{- |
Module      : Kore.Rewrite.SMT.Evaluator
Description : Uses a SMT solver for evaluating predicates.
Copyright   : (c) Runtime Verification, 2018-2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
-}
module Kore.Rewrite.SMT.Evaluator (
    decidePredicate,
    evalPredicate,
    evalConditional,
    filterMultiOr,
    getModelFor,
    translateTerm,
    translatePredicate,
) where

import Control.Error (
    MaybeT (..),
    runMaybeT,
 )
import Control.Exception (IOException)
import Control.Lens qualified as Lens
import Control.Monad.Catch qualified as Exception
import Control.Monad.Counter qualified as Counter
import Control.Monad.Morph qualified as Morph
import Control.Monad.State.Strict qualified as State
import Data.Generics.Product.Fields
import Data.Limit
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Attribute.Symbol qualified as Attribute (
    StepperAttributes,
    Symbol,
 )
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Conditional (
    Conditional,
 )
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.MultiOr (
    MultiOr,
 )
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.Predicate (
    Predicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.Substitution (Substitution)
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike (
    ElementVariable,
    ElementVariableName,
    SomeVariableName,
    Variable (..),
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Log.DebugEvaluateCondition (
    debugEvaluateConditionResult,
    whileDebugEvaluateCondition,
 )
import Kore.Log.DebugRetrySolverQuery (
    debugRetrySolverQuery,
 )
import Kore.Log.DecidePredicateUnknown (
    Loc,
    OnDecidePredicateUnknown (..),
    throwDecidePredicateUnknown,
 )
import Kore.Rewrite.SMT.Translate
import Kore.Simplify.Simplify as Simplifier
import Kore.TopBottom (
    TopBottom,
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified
import SMT (
    Result (..),
    SExpr (..),
    SMT,
 )
import SMT qualified
import SMT.SimpleSMT qualified as SimpleSMT

{- | Attempt to evaluate the 'Predicate' argument with an optional side
 condition using an external SMT solver.
-}
evalPredicate ::
    InternalVariable variable =>
    OnDecidePredicateUnknown ->
    Predicate variable ->
    Maybe (SideCondition variable) ->
    Simplifier (Maybe Bool)
evalPredicate onUnknown predicate sideConditionM = case predicate of
    Predicate.PredicateTrue -> return $ Just True
    Predicate.PredicateFalse -> return $ Just False
    _ -> case sideConditionM of
        Nothing ->
            predicate :| []
                & decidePredicate onUnknown SideCondition.top
        Just sideCondition ->
            predicate :| [from @_ @(Predicate _) sideCondition]
                & decidePredicate onUnknown sideCondition

{- | Attempt to evaluate the 'Conditional' argument with an optional side
 condition using an external SMT solver.
-}
evalConditional ::
    InternalVariable variable =>
    OnDecidePredicateUnknown ->
    Conditional variable term ->
    Maybe (SideCondition variable) ->
    Simplifier (Maybe Bool)
evalConditional onUnknown conditional sideConditionM =
    evalPredicate onUnknown predicate sideConditionM
        & assert (Conditional.isNormalized conditional)
  where
    predicate = case sideConditionM of
        Nothing -> Conditional.predicate conditional
        Just _ -> Condition.toPredicate $ Conditional.withoutTerm conditional

-- | Removes from a MultiOr all items refuted by an external SMT solver.
filterMultiOr ::
    forall term variable.
    ( Ord term
    , TopBottom term
    , InternalVariable variable
    ) =>
    Loc ->
    MultiOr (Conditional variable term) ->
    Simplifier (MultiOr (Conditional variable term))
filterMultiOr hsLoc multiOr = do
    elements <- mapM refute (toList multiOr)
    return (MultiOr.make (catMaybes elements))
  where
    refute ::
        Conditional variable term ->
        Simplifier (Maybe (Conditional variable term))
    refute p =
        evalConditional (WarnDecidePredicateUnknown hsLoc Nothing) p Nothing <&> \case
            Nothing -> Just p
            Just False -> Nothing
            Just True -> Just p

{- | Attempt to refute a predicate using an external SMT solver.

The predicate is always sent to the external solver, even if it is trivial.
-}
decidePredicate ::
    forall variable.
    InternalVariable variable =>
    OnDecidePredicateUnknown ->
    SideCondition variable ->
    NonEmpty (Predicate variable) ->
    Simplifier (Maybe Bool)
decidePredicate onUnknown sideCondition predicates =
    whileDebugEvaluateCondition predicates $
        do
            result <- query >>= whenUnknown retry
            debugEvaluateConditionResult result
            case result of
                Unsat -> return False
                Sat -> empty
                Unknown -> do
                    limit <- SMT.withSolver SMT.askRetryLimit
                    -- depending on the value of `onUnknown`, this call will either log a warning
                    -- or throw an error
                    throwDecidePredicateUnknown onUnknown limit predicates
                    case onUnknown of
                        WarnDecidePredicateUnknown _ _ ->
                            -- the solver may be in an inconsistent state, so we re-initialize
                            SMT.reinit
                        _ -> pure ()
                    empty
            & runMaybeT
  where
    query :: MaybeT Simplifier Result
    query = onErrorUnknown $ SMT.withSolver . evalTranslator $ do
        tools <- Simplifier.askMetadataTools
        Morph.hoist SMT.liftSMT $ do
            predicates' <-
                traverse
                    (translatePredicate sideCondition tools)
                    predicates
            traverse_ SMT.assert predicates'
            SMT.check >>= maybe empty return

    onErrorUnknown action =
        action `Exception.catch` \(_ :: IOException) -> pure Unknown

    retry = retryWithScaledTimeout $ query <* debugRetrySolverQuery predicates

retryWithScaledTimeout :: MonadSMT m => m Result -> m Result
retryWithScaledTimeout q = do
    SMT.RetryLimit limit <- SMT.askRetryLimit
    -- Use the same timeout for the first retry, since sometimes z3
    -- decides it doesn't want to work today and all we need is to
    -- retry it once.
    let timeoutScales = takeWithin limit [1 ..]
        retryActions = map (retryOnceWithScaledTimeout q) timeoutScales
        combineRetries r1 r2 = r1 >>= whenUnknown r2
    -- This works even if 'retryActions' is infinite, because the second
    -- argument to 'whenUnknown' will be the 'combineRetries' of all of
    -- the tail of the list. As soon as a result is not 'Unknown', the
    -- rest of the fold is discarded.
    foldr combineRetries (pure Unknown) retryActions
  where
    -- helpers for re-trying solver queries with increasing timeout
    retryOnceWithScaledTimeout :: MonadSMT m => m a -> Integer -> m a
    retryOnceWithScaledTimeout action scale =
        -- reinit with scaled timeout to override the original timeout
        SMT.localTimeOut (scaleTimeOut scale) $ SMT.reinit >> action

    scaleTimeOut :: Integer -> SMT.TimeOut -> SMT.TimeOut
    scaleTimeOut _ (SMT.TimeOut Unlimited) = SMT.TimeOut Unlimited
    scaleTimeOut n (SMT.TimeOut (Limit r)) = SMT.TimeOut (Limit (n * r))

whenUnknown :: Monad m => m Result -> Result -> m Result
whenUnknown f Unknown = f
whenUnknown _ result = return result

{- | Check if the given combination of predicates is satisfiable, and
  provide a satisfying substitution if so. Otherwise indicate with a
  boolean whether the solver was able to determine 'unsat' or returned
  an 'unknown' result.

  This means that if no SMT solver is configured, this function will
  always return 'Left False'.

  The solver query is retried with configurable scaled timeout. TODO
-}
getModelFor ::
    forall variable.
    InternalVariable variable =>
    (SmtMetadataTools Attribute.StepperAttributes) ->
    NonEmpty (Predicate variable) ->
    SMT (Either Bool (Substitution variable))
getModelFor tools predicates =
    fmap (fromMaybe (Left False)) . runMaybeT $ do
        (smtPredicates, translatorState) <-
            runTranslator $
                traverse (translatePredicate SideCondition.top tools) predicates
        let variables = freeVars translatorState
        result <-
            -- FIXME consider variables for uninterpreted terms, too
            lift . SMT.withSolver $ satQuery smtPredicates (Map.elems variables)
        case result of
            Left Unknown -> pure (Left False)
            Left Unsat -> pure (Left True)
            Left Sat -> pure (Left False) -- error "impossible!"
            Right mapping -> do
                let freeVarMap =
                        traverse (backTranslateWith tools translatorState) $
                            Map.compose mapping variables
                case freeVarMap of
                    Left errMsg -> do
                        traceM $ "[Error] in back-translation: " <> errMsg
                        -- FIXME error logging?
                        pure (Left False)
                    Right m -> pure . Right $ mkSubst m
  where
    satQuery ::
        NonEmpty SExpr -> -- predicates
        [SExpr] -> -- interesting variables
        SMT (Either Result (Map.Map SExpr SExpr))
    satQuery ps vars = do
        traverse_ SMT.assert ps
        satResult <- SMT.check
        case satResult of
            Nothing -> pure $ Left Unknown
            Just Unsat -> pure $ Left Unsat
            Just Unknown -> pure $ Left Unknown
            Just Sat ->
                if null vars -- no free variables, trivial case
                    then pure $ Right Map.empty
                    else do
                        mbMapping <- SMT.getValue vars
                        case mbMapping of
                            Nothing -> pure $ Left Unknown -- something went wrong in getValue
                            Just mapping -> pure . Right $ Map.fromList mapping

    mkSubst ::
        Map.Map (ElementVariable variable) (TermLike.TermLike variable) ->
        Substitution variable
    mkSubst = Substitution.fromMap . Map.mapKeys TermLike.mkSomeVariable

translatePredicate ::
    forall variable.
    InternalVariable variable =>
    SideCondition variable ->
    SmtMetadataTools Attribute.Symbol ->
    Predicate variable ->
    Translator variable SMT SExpr
translatePredicate sideCondition tools predicate =
    translatePredicateWith tools sideCondition translateTerm predicate

translateTerm ::
    forall variable.
    InternalVariable variable =>
    -- | type name
    SExpr ->
    -- | uninterpreted pattern
    TranslateItem variable ->
    Translator variable SMT SExpr
translateTerm smtType (QuantifiedVariable var) = do
    n <- Counter.increment
    let varName = "<" <> Text.pack (show n) <> ">"
        smtVar = SimpleSMT.const varName
    field @"quantifiedVars"
        Lens.%= Map.insert
            var
            SMTDependentAtom
                { smtName = varName
                , smtType
                , boundVars = []
                }
    return smtVar
translateTerm t (UninterpretedTerm (TermLike.ElemVar_ var)) =
    lookupVariable var <|> declareVariable t var
translateTerm t (UninterpretedPredicate predicate) = do
    TranslatorState{quantifiedVars, predicates} <- State.get
    let freeVars = FreeVariables.freeVariableNames @_ @variable predicate
        boundVarsMap = filterBoundVarsMap freeVars quantifiedVars
        boundPat =
            Predicate.makeExistsPredicateN (Map.keys boundVarsMap) predicate
        stateSetter = field @"predicates"
    lookupUninterpreted boundPat quantifiedVars predicates
        <|> declareUninterpreted t stateSetter boundPat boundVarsMap
translateTerm t (UninterpretedTerm pat) = do
    TranslatorState{quantifiedVars, terms} <- State.get
    let freeVars = FreeVariables.freeVariableNames @_ @variable pat
        boundVarsMap = filterBoundVarsMap freeVars quantifiedVars
        boundPat = TermLike.mkExistsN (Map.keys boundVarsMap) pat
        stateSetter = field @"terms"
    lookupUninterpreted boundPat quantifiedVars terms
        <|> declareUninterpreted t stateSetter boundPat boundVarsMap

declareUninterpreted ::
    ( InternalVariable variable
    , Ord termOrPredicate
    , Pretty termOrPredicate
    ) =>
    SExpr ->
    Lens.ASetter'
        (TranslatorState variable)
        (Map.Map termOrPredicate (SMTDependentAtom variable)) ->
    termOrPredicate ->
    Map.Map (ElementVariable variable) (SMTDependentAtom variable) ->
    Translator variable SMT SExpr
declareUninterpreted
    sExpr
    stateSetter
    boundPat
    boundVarsMap =
        do
            n <- Counter.increment
            logVariableAssignment n boundPat
            let smtName = "<" <> Text.pack (show n) <> ">"
                origName = Text.pack . show . Pretty.pretty $ boundPat
                (boundVars, bindings) = unzip $ Map.assocs boundVarsMap
                cached = SMTDependentAtom{smtName, smtType = sExpr, boundVars}
            _ <-
                SMT.declareFun
                    SMT.FunctionDeclaration
                        { name = Atom smtName
                        , inputSorts = smtType <$> bindings
                        , resultSort = sExpr
                        }
                    origName
            stateSetter Lens.%= Map.insert boundPat cached
            translateSMTDependentAtom boundVarsMap cached

filterBoundVarsMap ::
    Ord variable =>
    Set.Set (SomeVariableName variable) ->
    Map.Map
        (Variable (ElementVariableName variable))
        (SMTDependentAtom variable) ->
    Map.Map
        (Variable (ElementVariableName variable))
        (SMTDependentAtom variable)
filterBoundVarsMap freeVars quantifiedVars =
    Map.filterWithKey
        (\k _ -> inject (variableName k) `Set.member` freeVars)
        quantifiedVars

lookupUninterpreted ::
    (InternalVariable variable, Ord k) =>
    k ->
    Map.Map (ElementVariable variable) (SMTDependentAtom variable) ->
    Map.Map k (SMTDependentAtom variable) ->
    Translator variable SMT SExpr
lookupUninterpreted boundPat quantifiedVars terms =
    maybe empty (translateSMTDependentAtom quantifiedVars) $
        Map.lookup boundPat terms

lookupVariable ::
    InternalVariable variable =>
    TermLike.ElementVariable variable ->
    Translator variable SMT SExpr
lookupVariable var =
    lookupQuantifiedVariable <|> lookupFreeVariable
  where
    lookupQuantifiedVariable = do
        TranslatorState{quantifiedVars} <- State.get
        maybeToTranslator $
            SMT.Atom . smtName <$> Map.lookup var quantifiedVars
    lookupFreeVariable = do
        TranslatorState{freeVars} <- State.get
        maybeToTranslator $ Map.lookup var freeVars

declareVariable ::
    InternalVariable variable =>
    -- | type name
    SExpr ->
    -- | variable to be declared
    TermLike.ElementVariable variable ->
    Translator variable SMT SExpr
declareVariable t variable = do
    n <- Counter.increment
    let varName = "<" <> Text.pack (show n) <> ">"
        pat = TermLike.mkElemVar variable
        origName = Text.pack . show . Pretty.pretty $ pat
    var <- SMT.declare varName origName t
    field @"freeVars" Lens.%= Map.insert variable var
    logVariableAssignment n pat
    return var

logVariableAssignment ::
    Pretty pretty =>
    MonadLog m =>
    Counter.Natural ->
    -- | variable to be declared
    pretty ->
    Translator variable m ()
logVariableAssignment n pat =
    logDebug
        . Text.pack
        . show
        . Pretty.nest 4
        . Pretty.sep
        $ [Pretty.pretty n, "|->", Pretty.pretty pat]
