{- |
Module      : Kore.Rewrite.Axiom.EvaluationStrategy
Description : Various strategies for axiom/builtin-based simplification.
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Rewrite.Axiom.EvaluationStrategy (
    builtinEvaluation,
    definitionEvaluation,
    simplificationEvaluation,
    firstFullEvaluation,
    simplifierWithFallback,
    mkEvaluator,

    -- * For testing
    attemptEquationAndAccumulateErrors,
    attemptEquations,
) where

import Control.Monad.Except (
    ExceptT (..),
    runExceptT,
 )
import Data.EitherR (
    ExceptRT (..),
 )
import Data.Semigroup (
    Min (..),
 )
import Data.Text qualified as Text
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Equation qualified as Equation
import Kore.Equation.DebugEquation (
    AttemptEquationError,
 )
import Kore.Equation.DebugEquation qualified as Equation
import Kore.Equation.Equation (
    Equation,
 )
import Kore.Equation.Registry (PartitionedEquations (..), partitionEquations)
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.Symbol
import Kore.Internal.TermLike as TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify
import Kore.Simplify.Simplify qualified as AttemptedAxiom (
    AttemptedAxiom (..),
 )
import Kore.Unparser (
    unparse,
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

{- | Creates an evaluator for a function from the full set of rules
that define it.
-}
definitionEvaluation ::
    [Equation RewritingVariableName] ->
    BuiltinAndAxiomSimplifier
definitionEvaluation equations =
    BuiltinAndAxiomSimplifier $ \term condition -> do
        result <-
            attemptEquations
                (attemptEquationAndAccumulateErrors condition term)
                equations
        case result of
            Right results ->
                (return . Applied)
                    AttemptedAxiomResults
                        { results
                        , remainders = OrPattern.bottom
                        }
            Left minError ->
                case getMin <$> minError of
                    Just (Equation.WhileCheckRequires _) ->
                        (return . NotApplicableUntilConditionChanges)
                            (SideCondition.toRepresentation condition)
                    _ -> return NotApplicable

attemptEquationAndAccumulateErrors ::
    SideCondition RewritingVariableName ->
    TermLike RewritingVariableName ->
    Equation RewritingVariableName ->
    ExceptRT
        (OrPattern RewritingVariableName)
        Simplifier
        (Maybe (Min (AttemptEquationError RewritingVariableName)))
attemptEquationAndAccumulateErrors condition term equation =
    attemptEquation
  where
    attemptEquation =
        ExceptRT . ExceptT $
            Equation.attemptEquation
                condition
                term
                equation
                >>= either (return . Left . Just . Min) (fmap Right . apply)
    apply = Equation.applyEquation condition equation

attemptEquations ::
    Monoid error =>
    (Equation variable -> ExceptRT result Simplifier error) ->
    [Equation variable] ->
    Simplifier (Either error result)
attemptEquations accumulator equations =
    foldlM
        (\err equation -> mappend err <$> accumulator equation)
        mempty
        equations
        & runExceptRT
        & runExceptT

-- | Create an evaluator from a single simplification rule.
simplificationEvaluation ::
    Equation RewritingVariableName ->
    BuiltinAndAxiomSimplifier
simplificationEvaluation equation =
    BuiltinAndAxiomSimplifier $ \term condition -> do
        result <-
            Equation.attemptEquation
                condition
                term
                equation
        let apply = Equation.applyEquation condition equation
        case result of
            Right applied -> do
                results <- apply applied
                (return . Applied)
                    AttemptedAxiomResults
                        { results
                        , remainders = OrPattern.bottom
                        }
            Left err ->
                case err of
                    Equation.WhileCheckRequires _ ->
                        (return . NotApplicableUntilConditionChanges)
                            (SideCondition.toRepresentation condition)
                    _ -> return NotApplicable

{- | Creates an evaluator that choses the result of the first evaluator if it
returns Applicable, otherwise returns the result of the second.
-}
simplifierWithFallback ::
    BuiltinAndAxiomSimplifier ->
    BuiltinAndAxiomSimplifier ->
    BuiltinAndAxiomSimplifier
simplifierWithFallback first second =
    BuiltinAndAxiomSimplifier
        (applyFirstSimplifierThatWorks [first, second])

{- | Wraps an evaluator for builtins. Will fail with error if there is no result
on concrete patterns.
-}
builtinEvaluation ::
    BuiltinAndAxiomSimplifier ->
    BuiltinAndAxiomSimplifier
builtinEvaluation evaluator =
    BuiltinAndAxiomSimplifier (evaluateBuiltin evaluator)

evaluateBuiltin ::
    -- | Map from axiom IDs to axiom evaluators
    BuiltinAndAxiomSimplifier ->
    TermLike RewritingVariableName ->
    SideCondition RewritingVariableName ->
    Simplifier (AttemptedAxiom RewritingVariableName)
evaluateBuiltin
    (BuiltinAndAxiomSimplifier builtinEvaluator)
    patt
    sideCondition =
        do
            result <- builtinEvaluator patt sideCondition
            case result of
                AttemptedAxiom.NotApplicable
                    | App_ appHead children <- patt
                    , Just hook_ <- Text.unpack <$> Attribute.getHook (symbolHook appHead)
                    , all isValue children ->
                        (error . show . Pretty.vsep)
                            [ "Expecting hook "
                                <> Pretty.squotes (Pretty.pretty hook_)
                                <> " to reduce concrete pattern:"
                            , Pretty.indent 4 (unparse patt)
                            ]
                _ -> return result
      where
        isValue pat =
            maybe False TermLike.isConstructorLike $ asConcrete pat

-- | Creates an 'BuiltinAndAxiomSimplifier' from a set of equations.
mkEvaluator ::
    [Equation RewritingVariableName] ->
    Maybe BuiltinAndAxiomSimplifier
mkEvaluator equations =
    case (simplificationEvaluator, definitionEvaluator) of
        (Nothing, Nothing) -> Nothing
        (Just evaluator, Nothing) -> Just evaluator
        (Nothing, Just evaluator) -> Just evaluator
        (Just sEvaluator, Just dEvaluator) ->
            Just (simplifierWithFallback dEvaluator sEvaluator)
  where
    PartitionedEquations{functionRules, simplificationRules} = partitionEquations equations
    simplificationEvaluator =
        if null simplificationRules
            then Nothing
            else
                Just . firstFullEvaluation $
                    simplificationEvaluation
                        <$> simplificationRules
    definitionEvaluator =
        if null functionRules
            then Nothing
            else Just $ definitionEvaluation functionRules
