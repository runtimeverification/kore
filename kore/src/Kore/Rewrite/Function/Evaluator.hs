{- |
Module      : Kore.Rewrite.Function.Evaluator
Description : Evaluates functions in a pattern.
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Rewrite.Function.Evaluator (
    evaluateApplication,
    evaluatePattern,
) where

import Control.Error (
    ExceptT,
    MaybeT (..),
    exceptT,
    maybeT,
    throwE,
 )
import Control.Monad qualified as Monad
import Control.Monad.Trans.Reader (ReaderT)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Kore.Attribute.Pattern.Simplified qualified as Attribute.Simplified
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Attribute.Synthetic
import Kore.Builtin (koreEvaluators)
import Kore.Equation.DebugEquation (whileDebugTerm)
import Kore.Internal.MultiOr qualified as MultiOr (
    flatten,
    merge,
 )
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Condition,
    Conditional (..),
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.SideCondition.SideCondition qualified as SideCondition (
    Representation,
 )
import Kore.Internal.Symbol (isDeclaredFunction)
import Kore.Internal.Symbol qualified as Symbol
import Kore.Internal.TermLike as TermLike
import Kore.Log.DebugContext (inContext)
import Kore.Log.ErrorBottomTotalFunction (
    errorBottomTotalFunction,
 )
import Kore.Log.WarnFunctionWithoutEvaluators (warnFunctionWithoutEvaluators)
import Kore.Rewrite.Axiom.EvaluationStrategy (
    builtinEvaluation,
    mkEvaluator,
    simplifierWithFallback,
 )
import Kore.Rewrite.Axiom.Identifier (AxiomIdentifier)
import Kore.Rewrite.Axiom.Identifier qualified as Axiom.Identifier
import Kore.Rewrite.Function.Memo qualified as Memo
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify as AttemptedAxiom (
    AttemptedAxiom (..),
 )
import Kore.Simplify.Simplify as Simplifier
import Kore.TopBottom
import Kore.Unparser
import Logic qualified
import Prelude.Kore
import Pretty ((<+>))
import Pretty qualified

-- | Evaluates functions on an application pattern.

-- TODO (thomas.tuegel): Factor out a "function evaluator" object.
-- See also: Kore.Rewrite.Function.Memo.Self
-- Then add a function,
--   memoize :: Evaluator.Self state -> Memo.Self state -> Evaluator.Self state
-- to add memoization to a function evaluator.
evaluateApplication ::
    -- | The predicate from the configuration
    SideCondition RewritingVariableName ->
    -- | Aggregated children predicate and substitution.
    Condition RewritingVariableName ->
    -- | The pattern to be evaluated
    Application Symbol (TermLike RewritingVariableName) ->
    Simplifier (OrPattern RewritingVariableName)
evaluateApplication
    sideCondition
    childrenCondition
    (evaluateSortInjection -> application) = inContext "evaluateApplication" $
        finishT $ do
            -- for_ canMemoize recallOrPattern
            results <-
                inContext "evaluateApplication.evaluatePattern" $
                    evaluatePattern
                        sideCondition
                        childrenCondition
                        termLike
                        unevaluated
                        & maybeT (unevaluated Nothing) return
                        & lift
            -- for_ canMemoize (recordOrPattern results)
            let unexpectedBottomResult = Symbol.isTotal symbol && isBottom results
            when unexpectedBottomResult $
                lift $
                    errorBottomTotalFunction termLike
            inContext "evaluateApplication.return" $ return results
      where
        finishT :: ExceptT r Simplifier r -> Simplifier r
        finishT = exceptT return return

        Application{applicationSymbolOrAlias = symbol} = application

        termLike = synthesize (ApplySymbolF application)

        unevaluated ::
            MonadLog m =>
            Maybe SideCondition.Representation ->
            m (OrPattern RewritingVariableName)
        unevaluated maybeSideCondition =
            inContext "evaluateApplication.unevaluated" $
                return $
                    OrPattern.fromPattern $
                        Pattern.withCondition
                            (markSimplifiedIfChildren maybeSideCondition termLike)
                            childrenCondition

        markSimplifiedIfChildren ::
            Maybe SideCondition.Representation ->
            TermLike RewritingVariableName ->
            TermLike RewritingVariableName
        markSimplifiedIfChildren Nothing =
            TermLike.setSimplified
                (foldMap TermLike.simplifiedAttribute application)
        markSimplifiedIfChildren (Just condition) =
            TermLike.setSimplified
                ( foldMap TermLike.simplifiedAttribute application
                    <> Attribute.Simplified.simplifiedConditionally condition
                )

        canMemoize
            | Symbol.isMemo symbol
            , ( isTop childrenCondition
                    && isTop (SideCondition.toPredicate sideCondition)
              )
                || all TermLike.isConstructorLike application =
                traverse asConcrete application
            | otherwise =
                Nothing

        recallOrPattern key = do
            Memo.Self{recall} <- askMemo
            maybeTermLike <- recall key
            let maybeOrPattern =
                    OrPattern.fromTermLike . fromConcrete <$> maybeTermLike
            for_ maybeOrPattern throwE

        recordOrPattern orPattern key
            | [result] <- OrPattern.toPatterns orPattern
            , Just term <- asConcrete (Pattern.term result)
            , -- If the pattern and predicate are concrete, then we expect the predicate
              -- to be fully-evaluated, so it must be Top. It may not be fully-evaluated
              -- due to some uninterpreted function or an unsolved unification problem;
              -- these are not errors, but they are unusual enough that we don't want to
              -- deal with them here.
              isTop (Pattern.predicate result)
            , -- We already checked that childrenCondition has no substitutions, so we
              -- don't expect the result to have any substitutions either. As with the
              -- predicate, it might be possible to have a substitution in some cases,
              -- but they are unusual enough that we don't need to deal with them here.
              isTop (Pattern.substitution result) =
                do
                    Memo.Self{record} <- askMemo
                    record key term
            | otherwise =
                return ()

{- | Evaluates axioms on patterns.

Returns Nothing if there is no axiom for the pattern's identifier.
-}
evaluatePattern ::
    forall simplifier.
    MonadSimplify simplifier =>
    -- | The predicate from the configuration
    SideCondition RewritingVariableName ->
    -- | Aggregated children predicate and substitution.
    Condition RewritingVariableName ->
    -- | The pattern to be evaluated
    TermLike RewritingVariableName ->
    -- | The default value
    ( Maybe SideCondition.Representation ->
      MaybeT simplifier (OrPattern RewritingVariableName)
    ) ->
    MaybeT simplifier (OrPattern RewritingVariableName)
evaluatePattern
    !sideCondition
    !childrenCondition
    !termLike
    !defaultValue =
        inContext "Function.Evaluator.evaluatePattern" $ do
            BuiltinAndAxiomSimplifier evaluator <-
                inContext "evaluatePattern.lookupAxiomSimplifier" $ lookupAxiomSimplifier termLike
            whileDebugTerm termLike $ do
                merged <- do
                    result <- inContext "evaluatePattern.evaluator" $ liftSimplifier $ evaluator termLike sideCondition
                    flattened <- inContext "evaluatePattern.flatten" $ case result of
                        AttemptedAxiom.Applied
                            AttemptedAxiomResults
                                { results = orResults
                                , remainders = orRemainders
                                } -> do
                                simplified <- lift $ OrPattern.traverse simplifyIfNeeded orResults
                                let simplifiedResult = MultiOr.flatten simplified
                                return
                                    ( AttemptedAxiom.Applied
                                        AttemptedAxiomResults
                                            { results = simplifiedResult
                                            , remainders = orRemainders
                                            }
                                    )
                        _ -> return result
                    inContext "evaluatePattern.merge" $
                        liftSimplifier $
                            mergeWithConditionAndSubstitution
                                sideCondition
                                childrenCondition
                                flattened
                case merged of
                    AttemptedAxiom.NotApplicable ->
                        defaultValue Nothing
                    AttemptedAxiom.NotApplicableUntilConditionChanges c ->
                        defaultValue (Just c)
                    AttemptedAxiom.Applied attemptResults ->
                        return $ MultiOr.merge results remainders
                      where
                        AttemptedAxiomResults{results, remainders} =
                            attemptResults
      where
        unchangedPatt =
            Conditional
                { term = termLike
                , predicate = predicate
                , substitution = substitution
                }
          where
            Conditional{term = (), predicate, substitution} = childrenCondition

        simplifyIfNeeded ::
            Pattern RewritingVariableName ->
            simplifier (OrPattern RewritingVariableName)
        simplifyIfNeeded toSimplify
            | toSimplify == unchangedPatt =
                return (OrPattern.fromPattern unchangedPatt)
            | otherwise =
                liftSimplifier $ simplifyPattern sideCondition toSimplify
{-# SPECIALIZE evaluatePattern ::
    SideCondition RewritingVariableName ->
    Condition RewritingVariableName ->
    TermLike RewritingVariableName ->
    ( Maybe SideCondition.Representation ->
      MaybeT Simplifier (OrPattern RewritingVariableName)
    ) ->
    MaybeT Simplifier (OrPattern RewritingVariableName)
    #-}
{-# SPECIALIZE evaluatePattern ::
    SideCondition RewritingVariableName ->
    Condition RewritingVariableName ->
    TermLike RewritingVariableName ->
    ( Maybe SideCondition.Representation ->
      MaybeT (ReaderT (SideCondition RewritingVariableName) Simplifier) (OrPattern RewritingVariableName)
    ) ->
    MaybeT (ReaderT (SideCondition RewritingVariableName) Simplifier) (OrPattern RewritingVariableName)
    #-}

lookupAxiomSimplifier ::
    MonadSimplify simplifier =>
    TermLike RewritingVariableName ->
    MaybeT simplifier BuiltinAndAxiomSimplifier
lookupAxiomSimplifier termLike = do
    hookedSymbols <- lift askHookedSymbols
    axiomEquations <- lift askAxiomEquations
    let getEvaluator :: AxiomIdentifier -> Maybe BuiltinAndAxiomSimplifier
        getEvaluator axiomIdentifier = Map.lookup axiomIdentifier axiomEquations >>= mkEvaluator

        combineEvaluators :: [AxiomIdentifier] -> Maybe BuiltinAndAxiomSimplifier
        combineEvaluators identifiers =
            case mapMaybe getEvaluator identifiers of
                [] -> Nothing
                [a] -> Just a
                as -> Just $ firstFullEvaluation as

    let missing = do
            -- TODO (thomas.tuegel): Factor out a second function evaluator and
            -- remove this check. At startup, the definition's rules are
            -- simplified using Matching Logic only (no function
            -- evaluation). During this stage, all the hooks are expected to be
            -- missing, so that is not an error. If any function evaluators are
            -- present, we assume that startup is finished, but we should really
            -- have a separate evaluator for startup.
            Monad.guard (not $ null axiomEquations)
            case termLike of
                App_ symbol _
                    | isDeclaredFunction symbol -> do
                        let hooked = criticalMissingHook symbol
                            unhooked = warnFunctionWithoutEvaluators symbol
                        maybe unhooked hooked $ getHook symbol
                _ -> return ()
            empty
    maybe missing return $ do
        let axiomIdentifier = Axiom.Identifier.matchAxiomIdentifier termLike
        case axiomIdentifier of
            Axiom.Identifier.Application appId ->
                let builtinEvaluator = do
                        name <- Map.lookup appId hookedSymbols
                        builtinEvaluation <$> koreEvaluators name
                 in combineEvaluatorsWithFallBack (builtinEvaluator, getEvaluator axiomIdentifier)
            Axiom.Identifier.Ceil _ ->
                let inexact =
                        [ Axiom.Identifier.Ceil Axiom.Identifier.Variable
                        , Axiom.Identifier.Ceil Axiom.Identifier.Other
                        ]
                 in combineEvaluators $ axiomIdentifier : inexact
            Axiom.Identifier.Exists _ ->
                let inexact =
                        [ Axiom.Identifier.Exists Axiom.Identifier.Variable
                        , Axiom.Identifier.Exists Axiom.Identifier.Other
                        ]
                 in combineEvaluators $ axiomIdentifier : inexact
            Axiom.Identifier.Equals id1 id2 ->
                let inexact =
                        [ Axiom.Identifier.Equals Axiom.Identifier.Variable id2
                        , Axiom.Identifier.Equals Axiom.Identifier.Other id2
                        , Axiom.Identifier.Equals id1 Axiom.Identifier.Variable
                        , Axiom.Identifier.Equals id1 Axiom.Identifier.Other
                        , Axiom.Identifier.Equals Axiom.Identifier.Variable Axiom.Identifier.Variable
                        , Axiom.Identifier.Equals Axiom.Identifier.Other Axiom.Identifier.Variable
                        , Axiom.Identifier.Equals Axiom.Identifier.Variable Axiom.Identifier.Other
                        ]
                 in combineEvaluators $ axiomIdentifier : inexact
            Axiom.Identifier.Not _ ->
                let inexact =
                        [ Axiom.Identifier.Not Axiom.Identifier.Variable
                        , Axiom.Identifier.Not Axiom.Identifier.Other
                        ]
                 in combineEvaluators $ axiomIdentifier : inexact
            Axiom.Identifier.Variable -> getEvaluator axiomIdentifier
            Axiom.Identifier.DV -> getEvaluator axiomIdentifier
            Axiom.Identifier.Other -> getEvaluator axiomIdentifier
  where
    getHook :: Symbol -> Maybe Text
    getHook = Attribute.getHook . Attribute.hook . symbolAttributes

    combineEvaluatorsWithFallBack ::
        (Maybe BuiltinAndAxiomSimplifier, Maybe BuiltinAndAxiomSimplifier) ->
        Maybe BuiltinAndAxiomSimplifier
    combineEvaluatorsWithFallBack = \case
        (Nothing, eval2) -> eval2
        (eval1, Nothing) -> eval1
        (Just eval1, Just eval2) -> Just $ simplifierWithFallback eval1 eval2
{-# SPECIALIZE lookupAxiomSimplifier ::
    TermLike RewritingVariableName ->
    MaybeT Simplifier BuiltinAndAxiomSimplifier
    #-}
{-# SPECIALIZE lookupAxiomSimplifier ::
    TermLike RewritingVariableName ->
    MaybeT (ReaderT (SideCondition RewritingVariableName) Simplifier) BuiltinAndAxiomSimplifier
    #-}

criticalMissingHook :: Symbol -> Text -> a
criticalMissingHook symbol hookName =
    (error . show . Pretty.vsep)
        [ "Error: missing hook"
        , "Symbol"
        , Pretty.indent 4 (unparse symbol)
        , "declared with attribute"
        , Pretty.indent 4 (unparse attribute)
        , "We don't recognize that hook and it was not given any rules."
        , "Please open a feature request at"
        , Pretty.indent 4 "https://github.com/runtimeverification/haskell-backend/issues"
        , "and include the text of this message."
        , "Workaround: Give rules for" <+> unparse symbol
        ]
  where
    attribute = Attribute.hookAttribute hookName

evaluateSortInjection ::
    InternalVariable variable =>
    Application Symbol (TermLike variable) ->
    Application Symbol (TermLike variable)
evaluateSortInjection ap
    | Symbol.isSortInjection apHead =
        case apChild of
            App_ apHeadChild grandChildren
                | Symbol.isSortInjection apHeadChild ->
                    let ~(fromSort', toSort') = sortInjectionSorts apHeadChild
                        apHeadNew = updateSortInjectionSource apHead fromSort'
                        resultApp = apHeadNew grandChildren
                     in assert (toSort' == fromSort) resultApp
            _ -> ap
    | otherwise = ap
  where
    apHead = applicationSymbolOrAlias ap
    ~(fromSort, _) = sortInjectionSorts apHead
    ~apChild = sortInjectionChild ap
    updateSortInjectionSource head1 fromSort1 children =
        Application
            { applicationSymbolOrAlias =
                Symbol.coerceSortInjection head1 fromSort1 toSort1
            , applicationChildren = children
            }
      where
        ~(_, toSort1) = sortInjectionSorts head1

sortInjectionChild :: Unparse a => Application Symbol a -> a
sortInjectionChild application =
    case applicationChildren application of
        [child] -> child
        _ ->
            (error . show . Pretty.vsep)
                [ "Sort injection pattern"
                , Pretty.indent 4 (unparse application)
                , "should have one argument."
                ]

sortInjectionSorts :: Symbol -> (Sort, Sort)
sortInjectionSorts symbol =
    case symbolParams symbol of
        [fromSort, toSort] -> (fromSort, toSort)
        _ ->
            (error . show . Pretty.vsep)
                [ "Sort injection symbol"
                , Pretty.indent 4 (unparse symbol)
                , "should have two sort parameters."
                ]

-- | Ands the given condition-substitution to the given function evaluation.
mergeWithConditionAndSubstitution ::
    -- | Top level condition.
    SideCondition RewritingVariableName ->
    -- | Condition and substitution to add.
    Condition RewritingVariableName ->
    -- | AttemptedAxiom to which the condition should be added.
    AttemptedAxiom RewritingVariableName ->
    Simplifier (AttemptedAxiom RewritingVariableName)
mergeWithConditionAndSubstitution _ _ AttemptedAxiom.NotApplicable =
    return AttemptedAxiom.NotApplicable
mergeWithConditionAndSubstitution
    _
    _
    na@(AttemptedAxiom.NotApplicableUntilConditionChanges _) =
        return na
mergeWithConditionAndSubstitution
    sideCondition
    toMerge
    (AttemptedAxiom.Applied AttemptedAxiomResults{results, remainders}) =
        do
            evaluatedResults <- OrPattern.observeAllT $ do
                result <- Logic.scatter results
                simplifyCondition sideCondition $ Pattern.andCondition result toMerge
            evaluatedRemainders <- OrPattern.observeAllT $ do
                remainder <- Logic.scatter remainders
                simplifyCondition sideCondition (Pattern.andCondition remainder toMerge)
            return $
                AttemptedAxiom.Applied
                    AttemptedAxiomResults
                        { results = evaluatedResults
                        , remainders = evaluatedRemainders
                        }
