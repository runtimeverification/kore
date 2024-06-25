{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Simplify.Pattern (
    simplifyTopConfiguration,
    simplifyTopConfigurationDefined,
    simplify,
    makeEvaluate,
) where

import Control.Monad (
    (>=>),
 )
import Kore.Internal.Conditional qualified as Conditional
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
import Kore.Internal.Predicate (
    makeCeilPredicate,
 )
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition (
    addConditionWithReplacements,
    assumeDefined,
    top,
 )
import Kore.Internal.Substitution (
    toMap,
 )
import Kore.Internal.TermLike (
    pattern Exists_,
 )
import Kore.Log.DebugContext (inContext)
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Simplify.Simplify (
    Simplifier,
    liftSimplifier,
    simplifyCondition,
    simplifyTerm,
 )
import Kore.Substitute
import Logic qualified
import Prelude.Kore

-- | Simplifies the 'Pattern' and removes the exists quantifiers at the top.
simplifyTopConfiguration ::
    Pattern RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
simplifyTopConfiguration =
    simplify >=> return . removeTopExists

{- | Simplifies the 'Pattern', with the assumption that the term is defined,
and removes the exists quantifiers at the top.
-}
simplifyTopConfigurationDefined ::
    Pattern RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
simplifyTopConfigurationDefined configuration =
    maybe
        (return OrPattern.bottom)
        (worker definedConfiguration)
        sideCondition
  where
    worker patt condition =
        makeEvaluate condition patt
            >>= return . removeTopExists

    term = Conditional.term configuration
    sideCondition = SideCondition.assumeDefined term
    definedConfiguration =
        makeCeilPredicate term
            & from @_ @(Condition _)
            & Pattern.andCondition configuration

-- | Removes all existential quantifiers at the top of every 'Pattern''s 'term'.
removeTopExists ::
    OrPattern RewritingVariableName ->
    OrPattern RewritingVariableName
removeTopExists = OrPattern.map removeTopExistsWorker
  where
    removeTopExistsWorker ::
        Pattern RewritingVariableName ->
        Pattern RewritingVariableName
    removeTopExistsWorker p@Conditional{term = Exists_ _ _ quantified} =
        removeTopExistsWorker p{term = quantified}
    removeTopExistsWorker p = p

-- | Simplifies an 'Pattern', returning an 'OrPattern'.
simplify ::
    Pattern RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
simplify = makeEvaluate SideCondition.top

{- | Simplifies a 'Pattern' with a custom 'SideCondition'.
This should only be used when it's certain that the
'SideCondition' was not created from the 'Condition' of
the 'Pattern'.
-}
makeEvaluate ::
    SideCondition RewritingVariableName ->
    Pattern RewritingVariableName ->
    Simplifier (OrPattern RewritingVariableName)
makeEvaluate sideCondition =
    loop . OrPattern.fromPattern
  where
    loop input = do
        output <-
            OrPattern.traverse worker input
                & fmap OrPattern.flatten
        if input == output
            then pure output
            else loop output

    worker pattern' =
        OrPattern.observeAllT $ do
            withSimplifiedCondition <-
                inContext "makeEvaluate.simplifyCondition-initial" $ simplifyCondition sideCondition pattern'
            let (term, simplifiedCondition) =
                    Conditional.splitTerm withSimplifiedCondition
                term' = substitute (toMap $ substitution simplifiedCondition) term
                termSideCondition =
                    SideCondition.addConditionWithReplacements
                        simplifiedCondition
                        sideCondition
            simplifiedTerm <-
                inContext "makeEvaluate.simplifyTerm" $
                    liftSimplifier (simplifyTerm termSideCondition term')
                        >>= Logic.scatter
            let simplifiedPattern =
                    Conditional.andCondition simplifiedTerm simplifiedCondition
            inContext "makeEvaluate.simplifyCondition-final" $ simplifyCondition sideCondition simplifiedPattern
