{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Equation.Simplification (
    simplifyEquation,
    simplifyExtractedEquations,
) where

import Data.Map.Strict (
    Map,
 )
import Kore.Equation.Equation
import Kore.Internal.Conditional (
    Conditional (..),
    fromPredicate,
 )
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.MultiAnd qualified as MultiAnd
import Kore.Internal.Predicate (makeAndPredicate)
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike qualified as TermLike
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify (
    Simplifier,
 )
import Kore.Simplify.Simplify qualified as Simplifier
import Kore.Substitute
import Logic qualified
import Prelude.Kore

{- | Simplify a 'Map' of 'Equation's using only Matching Logic rules.

See also: 'Kore.Equation.Registry.extractEquations'
-}
simplifyExtractedEquations ::
    Map identifier [Equation RewritingVariableName] ->
    Simplifier (Map identifier [Equation RewritingVariableName])
simplifyExtractedEquations = do
    results <- (traverse . traverse) simplifyEquation
    return $ collectResults results
  where
    collectResults = (fmap . fmap) (concatMap toList)

{- | Simplify an 'Equation' using only Matching Logic rules.

It attempts to unify the argument of the equation, creating new
equations where the argument is substituted in the rest of the
resulting equations, and the argument is removed.

If any of the patterns resulting from simplifying the term and the
argument contain a predicate which is not 'Top', 'simplifyEquation'
returns the original equation.
-}
simplifyEquation ::
    Equation RewritingVariableName ->
    Simplifier (MultiAnd (Equation RewritingVariableName))
simplifyEquation equation@(Equation _ _ _ _ _ _ _) =
    do
        simplifiedCond <-
            Simplifier.simplifyCondition
                SideCondition.top
                (fromPredicate $ makeAndPredicate argument' antiLeft')
        let Conditional{substitution, predicate} = simplifiedCond
            subst = Substitution.toMap substitution
            left' = substitute subst left
            requires' = makeAndPredicate (substitute subst requires) predicate
            right' = substitute subst right
            ensures' = substitute subst ensures
        return
            Equation
                { left = TermLike.forgetSimplified left'
                , requires = Predicate.forgetSimplified requires'
                , argument = Nothing
                , antiLeft = Nothing
                , right = TermLike.forgetSimplified right'
                , ensures = Predicate.forgetSimplified ensures'
                , attributes = attributes
                }
        & Logic.observeAllT
        & fmap MultiAnd.make
  where
    argument' =
        fromMaybe Predicate.makeTruePredicate argument
    antiLeft' =
        fromMaybe Predicate.makeTruePredicate antiLeft
    Equation
        { requires
        , argument
        , antiLeft
        , left
        , right
        , ensures
        , attributes
        } = equation
