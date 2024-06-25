{- |
Module      : Kore.Simplify.Application
Description : Tools for Application pattern simplification.
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Simplify.Application (
    simplify,
    Application (..),
) where

import Kore.Attribute.Synthetic (synthesize)
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Conditional (..),
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.TermLike
import Kore.Log.DebugContext (inContext)
import Kore.Rewrite.Function.Evaluator (
    evaluateApplication,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Rewrite.Substitution (
    mergePredicatesAndSubstitutions,
 )
import Kore.Simplify.Simplify as Simplifier
import Logic (
    LogicT,
 )
import Logic qualified
import Prelude.Kore

type ExpandedApplication variable =
    Conditional variable (Application Symbol (TermLike variable))

{- | 'simplify' simplifies an 'Application' of 'OrPattern'.

To do that, it first distributes the terms, making it an Or of Application
patterns, each Application having 'Pattern's as children,
then it simplifies each of those.

Simplifying an Application of Pattern means merging the children
predicates ans substitutions, applying functions on the Application(terms),
then merging everything into an Pattern.
-}
simplify ::
    SideCondition RewritingVariableName ->
    Application Symbol (OrPattern RewritingVariableName) ->
    Simplifier (OrPattern RewritingVariableName)
simplify sideCondition application = inContext "simplifyApplication" $ do
    evaluated <- OrPattern.observeAllT $ do
        Application{applicationChildren = result} <-
            Logic.scatter childrenCrossProduct
        lift $ makeAndEvaluateApplications sideCondition symbol result
    return (OrPattern.flatten evaluated)
  where
    Application
        { applicationSymbolOrAlias = symbol
        } =
            application

    -- The "Propagation Or" inference rule together with
    -- "Propagation Bottom" for the case when a child or is empty.
    childrenCrossProduct =
        MultiOr.distributeApplication application

makeAndEvaluateApplications ::
    SideCondition RewritingVariableName ->
    Symbol ->
    [Pattern RewritingVariableName] ->
    Simplifier (OrPattern RewritingVariableName)
makeAndEvaluateApplications =
    makeAndEvaluateSymbolApplications

makeAndEvaluateSymbolApplications ::
    SideCondition RewritingVariableName ->
    Symbol ->
    [Pattern RewritingVariableName] ->
    Simplifier (OrPattern RewritingVariableName)
makeAndEvaluateSymbolApplications sideCondition symbol children = do
    expandedApplications <-
        makeExpandedApplication sideCondition symbol children
            & Logic.observeAllT
    orResults <-
        traverse
            (evaluateApplicationFunction sideCondition)
            expandedApplications
    return (MultiOr.mergeAll orResults)

{- | Evaluates function applications, without attempting
 to reevaluate functions which are known to have been simplified
 as much as possible inside the current rewrite step.
-}
evaluateApplicationFunction ::
    -- | The predicate from the configuration
    SideCondition RewritingVariableName ->
    -- | The pattern to be evaluated
    ExpandedApplication RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
evaluateApplicationFunction
    sideCondition
    expandedApp@Conditional{term, predicate, substitution}
        | SideCondition.isSimplifiedFunction term sideCondition =
            inContext "evaluateApplicationFunction.one" $
                let applicationPattern =
                        synthesize . ApplySymbolF <$> expandedApp
                 in applicationPattern
                        & Pattern.markSimplified
                        & OrPattern.fromPattern
                        & return
        | otherwise =
            inContext "evaluateApplicationFunction.two" $
                evaluateApplication
                    sideCondition
                    Conditional{term = (), predicate, substitution}
                    term

makeExpandedApplication ::
    SideCondition RewritingVariableName ->
    Symbol ->
    [Pattern RewritingVariableName] ->
    LogicT Simplifier (ExpandedApplication RewritingVariableName)
makeExpandedApplication sideCondition symbol children = do
    merged <-
        mergePredicatesAndSubstitutions
            sideCondition
            (fmap Pattern.predicate children)
            (fmap Pattern.substitution children)
    let term =
            symbolApplication
                symbol
                (fmap Pattern.term children)

    return $ Conditional.withCondition term merged
