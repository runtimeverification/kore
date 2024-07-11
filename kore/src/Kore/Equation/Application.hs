{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Equation.Application (
    attemptEquation,
    AttemptEquationResult,
    applyEquation,
    applySubstitutionAndSimplify,
) where

import Control.Error (
    ExceptT (..),
    MaybeT (..),
    maybeToList,
    runExceptT,
    throwE,
    withExceptT,
 )
import Control.Monad (
    (>=>),
 )
import Control.Monad.Reader (asks)
import Control.Monad.State (modify)
import Data.Bifunctor
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Sequence ((|>))
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Pattern.FreeVariables (
    HasFreeVariables (..),
 )
import Kore.Attribute.Pattern.FreeVariables qualified as FreeVariables
import Kore.Equation.DebugEquation (
    ApplyMatchResultError (..),
    ApplyMatchResultErrors (..),
    AttemptEquationError (..),
    AttemptEquationResult,
    CheckRequiresError (..),
    MatchError (..),
    debugApplyEquation,
    debugAttemptEquationResult,
    whileApplyMatchResult,
    whileCheckRequires,
    whileDebugAttemptEquation,
    whileMatch,
 )
import Kore.Equation.DebugEquation qualified as Equation
import Kore.Equation.Equation (
    Equation (..),
 )
import Kore.Equation.Equation qualified as Equation
import Kore.Internal.Condition (
    Condition,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.OrCondition qualified as OrCondition
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Conditional (..),
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    Predicate,
    makeAndPredicate,
    makeNotPredicate,
 )
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.Substitution (
    Substitution,
 )
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike (
    TermLike,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Log.DebugContext (SimpleContext (CtxConstraint), inContext)
import Kore.Log.DecidePredicateUnknown (
    OnDecidePredicateUnknown (..),
    srcLoc,
 )
import Kore.Rewrite.Axiom.Matcher (
    MatchResult,
    patternMatch,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Rewrite.SMT.Evaluator qualified as SMT
import Kore.Rewrite.Substitution qualified as Substitution
import Kore.Simplify.Simplify (
    Simplifier,
    SimplifierTrace (..),
    liftSimplifier,
 )
import Kore.Simplify.Simplify qualified as Simplifier
import Kore.Substitute
import Kore.Syntax.Variable
import Kore.TopBottom
import Logic qualified
import Prelude.Kore

{- | Attempt to apply an 'Equation' to the 'TermLike'.

The 'SideCondition' is used to evaluate the 'requires' clause of the 'Equation'.

The caller should use 'debugApplyEquation' to log when the result of an
equation is actually used; @attemptEquation@ will only log when an equation is
applicable.
-}
attemptEquation ::
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Equation RewritingVariableName ->
    Simplifier (AttemptEquationResult RewritingVariableName)
attemptEquation sideCondition termLike equation = do
    result <- runMaybeT alreadyAttempted
    case result of
        Just attemptResult -> return (Left attemptResult)
        Nothing -> attemptEquationWorker
  where
    attemptEquationWorker =
        whileDebugAttemptEquation' . runExceptT $ do
            let Equation{left} = equationRenamed
            (equation', predicate) <- matchAndApplyResults left
            let Equation
                    { requires
                    , ensures
                    , right
                    , attributes = Attribute.Axiom{simplification}
                    } = equation'
                eqSrc = Equation.srcLoc equation'
                onDecidePredicateUnknown = case simplification of
                    Attribute.NotSimplification -> ErrorDecidePredicateUnknown $srcLoc eqSrc
                    Attribute.IsSimplification _ -> WarnDecidePredicateUnknown $srcLoc eqSrc
            checkRequires onDecidePredicateUnknown sideCondition predicate requires
                & whileCheckRequires
            return $ Pattern.withCondition right $ from @(Predicate _) ensures

    equationRenamed = refreshVariables sideCondition termLike equation
    matchError matchFailReason =
        MatchError
            { matchTerm = termLike
            , matchEquation = equationRenamed
            , matchFailReason
            }
    match term1 term2 =
        patternMatch sideCondition term1 term2
            & ExceptT
            & withExceptT matchError

    matchAndApplyResults left' = do
        matchResult <- match left' termLike & whileMatch
        applyMatchResult equationRenamed matchResult
            & whileApplyMatchResult

    whileDebugAttemptEquation' ::
        Simplifier (AttemptEquationResult RewritingVariableName) ->
        Simplifier (AttemptEquationResult RewritingVariableName)
    whileDebugAttemptEquation' action =
        whileDebugAttemptEquation termLike equationRenamed $ do
            result <- action
            cacheIfFailure result
            debugAttemptEquationResult equation result
            return result

    cacheIfFailure result =
        case result of
            Left failure ->
                addToCache failure
            _ -> return ()

    addToCache result = do
        oldCache <- Simplifier.getCache
        let newEntry =
                Simplifier.EvaluationAttempt
                    { cachedEquation = equation
                    , cachedTerm = termLike
                    }
            newCache =
                Simplifier.updateCache newEntry result oldCache
        Simplifier.putCache newCache

    alreadyAttempted = do
        cache <- Simplifier.getCache
        let entry =
                Simplifier.EvaluationAttempt
                    { cachedEquation = equation
                    , cachedTerm = termLike
                    }
        value <-
            Simplifier.lookupCache entry cache
                & (MaybeT . return)
        checkWithSideCondition value
      where
        checkWithSideCondition value =
            case value of
                WhileMatch _ -> return value
                WhileApplyMatchResult _ -> return value
                WhileCheckRequires
                    ( CheckRequiresError
                            { sideCondition = oldSideCondition
                            }
                        ) ->
                        if sideCondition == oldSideCondition
                            then return value
                            else empty

{- | Simplify the argument of a function definition equation with the
 match substitution and the priority predicate. This will avoid
 evaluating any function applications or builtins present in
 the predicates. It will not attempt any user defined simplification rules
 either.
-}
applySubstitutionAndSimplify ::
    HasCallStack =>
    Maybe (Predicate RewritingVariableName) ->
    Maybe (Predicate RewritingVariableName) ->
    Map
        (SomeVariableName RewritingVariableName)
        (TermLike RewritingVariableName) ->
    ExceptT
        (MatchError RewritingVariableName)
        Simplifier
        [MatchResult RewritingVariableName]
applySubstitutionAndSimplify
    argument
    antiLeft
    matchSubstitution =
        lift . Simplifier.localAxiomEquations mempty $ do
            let toMatchResult Conditional{predicate, substitution} =
                    (predicate, Substitution.toMap substitution)
            Substitution.mergePredicatesAndSubstitutions
                SideCondition.top
                (maybeToList argument <> maybeToList antiLeft)
                [from @_ @(Substitution _) matchSubstitution]
                & Logic.observeAllT
                & (fmap . fmap) toMatchResult

applyEquation ::
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Equation RewritingVariableName ->
    Pattern RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
applyEquation _ term equation result = do
    let results = OrPattern.fromPattern result
    debugApplyEquation equation result
    doTracing <- liftSimplifier $ asks Simplifier.tracingEnabled
    when doTracing $
        modify $
            second (|> SimplifierTrace term (Attribute.uniqueId $ attributes equation) result)
    pure results

{- | Use a 'MatchResult' to instantiate an 'Equation'.

The 'MatchResult' must cover all the free variables of the 'Equation'; this
condition is not checked, but enforced by the matcher. The result is the
'Equation' and any 'Predicate' assembled during matching, both instantiated by
the 'MatchResult'.

Throws 'ApplyMatchResultErrors' if there is a problem with the 'MatchResult'.
-}
applyMatchResult ::
    forall monad.
    Monad monad =>
    Equation RewritingVariableName ->
    MatchResult RewritingVariableName ->
    ExceptT
        (ApplyMatchResultErrors RewritingVariableName)
        monad
        (Equation RewritingVariableName, Predicate RewritingVariableName)
applyMatchResult equation matchResult@(predicate, substitution) = do
    case errors of
        x : xs ->
            throwE
                ApplyMatchResultErrors
                    { matchResult
                    , applyMatchErrors = x :| xs
                    }
        _ -> return ()
    let predicate' = substitute orientedSubstitution predicate
        equation' = substitute orientedSubstitution equation
    return (equation', predicate')
  where
    orientedSubstitution = Substitution.orientSubstitution occursInEquation substitution

    equationVariables = freeVariables equation

    occursInEquation :: (SomeVariableName RewritingVariableName -> Bool)
    occursInEquation = \someVariableName ->
        Set.member someVariableName equationVariableNames

    equationVariableNames =
        Set.mapMonotonic variableName (FreeVariables.toSet equationVariables)

    errors =
        concatMap checkVariable (FreeVariables.toList equationVariables)
            <> checkNotInEquation

    checkVariable Variable{variableName} =
        case Map.lookup variableName orientedSubstitution of
            Nothing -> [NotMatched variableName]
            Just termLike ->
                checkConcreteVariable variableName termLike
                    <> checkSymbolicVariable variableName termLike

    checkConcreteVariable variable termLike
        | Set.member variable concretes
        , (not . TermLike.isConstructorLike) termLike =
            [NotConcrete variable termLike]
        | otherwise =
            empty

    checkSymbolicVariable variable termLike
        | Set.member variable symbolics
        , TermLike.isConstructorLike termLike =
            [NotSymbolic variable termLike]
        | otherwise =
            empty

    checkNotInEquation =
        NonMatchingSubstitution
            <$> filter (not . occursInEquation) (Map.keys orientedSubstitution)

    Equation{attributes} = equation
    concretes =
        attributes
            & Attribute.concrete
            & from @_ @(Set _)
    symbolics =
        attributes
            & Attribute.symbolic
            & from @_ @(Set _)

{- | Check that the requires from matching and the 'Equation' hold.

Throws 'RequiresNotMet' if the 'Predicate's do not hold under the
'SideCondition'.
-}
checkRequires ::
    OnDecidePredicateUnknown ->
    SideCondition RewritingVariableName ->
    -- | requires from matching
    Predicate RewritingVariableName ->
    -- | requires from 'Equation'
    Predicate RewritingVariableName ->
    ExceptT (CheckRequiresError RewritingVariableName) Simplifier ()
checkRequires onUnknown sideCondition predicate requires =
    inContext CtxConstraint $
        do
            let requires' = makeAndPredicate predicate requires
                -- The condition to refute:
                condition :: Condition RewritingVariableName
                condition = from @(Predicate _) (makeNotPredicate requires')
            return condition
                -- First try to refute 'condition' without user-defined axioms:
                >>= withoutAxioms . simplifyCondition
                -- Next try to refute 'condition' including user-defined axioms:
                >>= withAxioms . simplifyCondition
                -- Finally, try to refute the simplified 'condition' using the
                -- external solver:
                >>= filterBranch

            -- Collect the simplified results. If they are \bottom, then \and(predicate,
            -- requires) is valid; otherwise, the required pre-conditions are not met
            -- and the rule will not be applied.
            & (OrCondition.observeAllT >=> assertBottom)
  where
    simplifyCondition = Simplifier.simplifyCondition sideCondition

    filterBranch condition = do
        l <-
            liftSimplifier $
                SMT.evalConditional
                    onUnknown
                    condition
                    (Just sideCondition)
        case l of
            Just False -> empty
            _ -> return condition

    assertBottom negatedImplication
        | isBottom negatedImplication = done
        | otherwise = requiresNotMet negatedImplication
    done = return ()
    requiresNotMet negatedImplication =
        throwE
            CheckRequiresError
                { matchPredicate = predicate
                , equationRequires = requires
                , sideCondition
                , negatedImplication
                }

    withoutAxioms =
        fmap Condition.forgetSimplified
            . Simplifier.localAxiomEquations (const mempty)
    withAxioms = id

refreshVariables ::
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Equation RewritingVariableName ->
    Equation RewritingVariableName
refreshVariables sideCondition initial =
    snd
        . Equation.refreshVariables avoiding
  where
    avoiding = sideConditionVariables <> freeVariables initial
    sideConditionVariables = freeVariables sideCondition
