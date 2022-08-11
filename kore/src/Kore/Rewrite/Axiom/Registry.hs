{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Rewrite.Axiom.Registry (
    mkEvaluator,
    mkEvaluatorRegistry,
    extractEquations,
) where

import Data.Map.Strict (
    Map,
 )
import Kore.Equation (
    Equation (..),
 )
import Prelude.Kore

-- TODO (thomas.tuegel): Remove private import
import Kore.Equation.Registry
import Kore.Rewrite.Axiom.EvaluationStrategy (
    definitionEvaluation,
    firstFullEvaluation,
    simplificationEvaluation,
    simplifierWithFallback,
 )
import Kore.Rewrite.Axiom.Identifier (
    AxiomIdentifier,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify (
    BuiltinAndAxiomSimplifier (..),
 )

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

mkEvaluatorRegistry ::
    Map AxiomIdentifier [Equation RewritingVariableName] ->
    Map AxiomIdentifier BuiltinAndAxiomSimplifier
mkEvaluatorRegistry =
    mapMaybe mkEvaluator
