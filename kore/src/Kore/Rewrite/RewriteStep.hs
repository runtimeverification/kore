{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause

Direct interface to rule application (step-wise execution).
See "Kore.Rewrite" for the high-level strategy-based interface.
-}
module Kore.Rewrite.RewriteStep (
    applyRewriteRulesParallel,
    withoutUnification,
    applyRewriteRulesSequence,
    applyClaimsSequence,
    EnableAssumeInitialDefined (..),
) where

import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Class qualified as Monad.Trans
import Data.Sequence qualified as Seq
import Kore.Attribute.Label (
    Label (..),
 )
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
 )
import Kore.Attribute.SourceLocation (
    SourceLocation,
 )
import Kore.Attribute.UniqueId (
    UniqueId,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.Conditional qualified as Conditional
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern as Pattern
import Kore.Internal.SideCondition (SideCondition)
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike as TermLike
import Kore.Log.DebugAppliedRewriteRules (
    debugAppliedRewriteRules,
 )
import Kore.Log.DebugCreatedSubstitution (debugCreatedSubstitution)
import Kore.Log.DebugRewriteRulesRemainder (debugRewriteRulesRemainder)
import Kore.Log.DebugRewriteTrace (
    debugRewriteTrace,
 )
import Kore.Log.DecidePredicateUnknown (OnDecidePredicateUnknown (..), srcLoc)
import Kore.Log.ErrorRewritesInstantiation (
    checkSubstitutionCoverage,
 )
import Kore.Rewrite.AxiomPattern (
    AxiomPattern,
 )
import Kore.Rewrite.ClaimPattern (
    ClaimPattern (..),
 )
import Kore.Rewrite.ClaimPattern qualified as Claim
import Kore.Rewrite.Remainder qualified as Remainder
import Kore.Rewrite.Result qualified as Result
import Kore.Rewrite.Result qualified as Step
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.RulePattern (
    RewriteRule (..),
    RulePattern,
 )
import Kore.Rewrite.RulePattern qualified as Rule
import Kore.Rewrite.SMT.Evaluator qualified as SMT
import Kore.Rewrite.Step (
    Result,
    Results,
    UnifiedRule,
    applyInitialConditions,
    applyRemainder,
    assertFunctionLikeResults,
    unifyRules,
 )
import Kore.Simplify.Ceil (
    enumerateSubtermsNeedingCeil,
 )
import Kore.Simplify.Simplify (
    Simplifier,
    simplifyCondition,
 )
import Kore.Substitute
import Logic (
    LogicT,
 )
import Logic qualified
import Prelude.Kore

withoutUnification :: UnifiedRule rule -> rule
withoutUnification = Conditional.term

{- | Produce the final configurations of an applied rule.

The rule's 'ensures' clause is applied to the conditions and normalized. The
substitution is applied to the right-hand side of the rule to produce the final
configurations.

See also: 'applyInitialConditions'
-}
finalizeAppliedRule ::
    -- | SideCondition containing metadata
    SideCondition RewritingVariableName ->
    -- | Applied rule
    RulePattern RewritingVariableName ->
    -- | Conditions of applied rule
    Condition RewritingVariableName ->
    LogicT
        Simplifier
        ( Substitution.Substitution
            RewritingVariableName
        , Pattern RewritingVariableName
        )
finalizeAppliedRule
    sideCondition
    renamedRule
    appliedCondition = do
        let avoidVars = freeVariables appliedCondition <> freeVariables ruleRHS
            finalPattern =
                Rule.topExistsToImplicitForall avoidVars ruleRHS

        -- `constructConfiguration` may simplify the configuration to bottom
        -- we want to "catch" this and return a #bottom Pattern
        catchSimplifiesToBottom (termLikeSort $ term finalPattern)
            =<< Logic.gather
                ( -- Combine the initial conditions, the unification conditions, and the
                  -- axiom ensures clause. The axiom requires clause is included by
                  -- unifyRule.
                  constructConfiguration
                    sideCondition
                    appliedCondition
                    finalPattern
                )
      where
        ruleRHS = Rule.rhs renamedRule

        catchSimplifiesToBottom srt = \case
            [] -> return (mempty, pure $ mkBottom srt)
            xs -> Logic.scatter xs

{- | Combine all the conditions to apply rule and construct the result.

@constructConfiguration@ combines:

* the applied condition (the initial condition, unification condition, and the
  rule's @requires@ clause), and
* the rule's @ensures@ clause

The parts of the applied condition were already combined by 'unifyRule'. First, the @ensures@ clause is simplified under the applied condition. Then, the conditions are conjoined and simplified again.
-}
constructConfiguration ::
    -- | SideCondition containing metadata
    SideCondition RewritingVariableName ->
    -- | Applied condition
    Condition RewritingVariableName ->
    -- | Final configuration
    Pattern RewritingVariableName ->
    LogicT
        Simplifier
        ( Substitution.Substitution
            RewritingVariableName
        , Pattern RewritingVariableName
        )
constructConfiguration
    sideCondition
    appliedCondition
    finalPattern = do
        let ensuresCondition = Pattern.withoutTerm finalPattern
        finalCondition <-
            do
                partial <-
                    simplifyCondition
                        ( sideCondition
                            & SideCondition.addConditionWithReplacements
                                appliedCondition
                        )
                        ensuresCondition
                -- TODO (thomas.tuegel): It should not be necessary to simplify
                -- after conjoining the conditions.
                simplifyCondition sideCondition (appliedCondition <> partial)
                & Logic.lowerLogicT
        -- Apply the normalized substitution to the right-hand side of the
        -- axiom.
        let Conditional{substitution} = finalCondition
            substitution' = Substitution.toMap substitution
            Conditional{term = finalTerm} = finalPattern
            finalTerm' = substitute substitution' finalTerm
        -- TODO (thomas.tuegel): Should the final term be simplified after
        -- substitution?
        debugCreatedSubstitution substitution (termLikeSort finalTerm)
        return (substitution, finalTerm' `Pattern.withCondition` finalCondition)

finalizeAppliedClaim ::
    -- | SideCondition containing metadata
    SideCondition RewritingVariableName ->
    -- | Applied rule
    ClaimPattern ->
    -- | Conditions of applied rule
    Condition RewritingVariableName ->
    LogicT Simplifier (Substitution.Substitution RewritingVariableName, Pattern RewritingVariableName)
finalizeAppliedClaim sideCondition renamedRule appliedCondition =
    -- `constructConfiguration` may simplify the configuration to bottom
    -- we want to "catch" this and return a #bottom Pattern
    catchSimplifiesToBottom (Claim.getClaimPatternSort renamedRule)
        =<< Logic.gather
            ( Claim.assertRefreshed renamedRule $ do
                finalPattern <- Logic.scatter right
                -- Combine the initial conditions, the unification conditions, and
                -- the axiom ensures clause. The axiom requires clause is included
                -- by unifyRule.
                constructConfiguration
                    sideCondition
                    appliedCondition
                    finalPattern
            )
  where
    ClaimPattern{right} = renamedRule

    catchSimplifiesToBottom srt = \case
        [] -> return (mempty, pure $ mkBottom srt)
        xs -> Logic.scatter xs

type UnifyingRuleWithRepresentation representation rule =
    ( Rule.UnifyingRule representation
    , Rule.UnifyingRuleVariable representation ~ RewritingVariableName
    , Rule.UnifyingRule rule
    , Rule.UnifyingRuleVariable rule ~ RewritingVariableName
    , From rule (AxiomPattern RewritingVariableName)
    , From rule SourceLocation
    , From rule UniqueId
    )

type FinalizeApplied rule =
    rule ->
    Condition RewritingVariableName ->
    LogicT Simplifier (Substitution.Substitution RewritingVariableName, Pattern RewritingVariableName)

finalizeRule ::
    UnifyingRuleWithRepresentation representation rule =>
    SideCondition RewritingVariableName ->
    (representation -> rule) ->
    FinalizeApplied representation ->
    FreeVariables RewritingVariableName ->
    -- | Initial conditions
    Pattern RewritingVariableName ->
    -- | Rewriting axiom
    UnifiedRule representation ->
    Simplifier [Result representation]
finalizeRule
    sideCondition
    toRule
    finalizeApplied
    initialVariables
    initial
    unifiedRule =
        Logic.observeAllT $ do
            let initialCondition = Conditional.withoutTerm initial
            let unificationCondition = Conditional.withoutTerm unifiedRule
            applied <-
                applyInitialConditions
                    sideCondition
                    initialCondition
                    unificationCondition
            checkSubstitutionCoverage initial (toRule <$> unifiedRule)
            let renamedRule = Conditional.term unifiedRule
            (subst, final) <- finalizeApplied renamedRule applied
            let result = resetResultPattern initialVariables final
            return Step.Result{appliedRule = unifiedRule{substitution = subst}, result}

-- | Finalizes a list of applied rules into 'Results'.
type Finalizer rule =
    FreeVariables RewritingVariableName ->
    Pattern RewritingVariableName ->
    [UnifiedRule rule] ->
    Simplifier (Results rule)

finalizeRulesParallel ::
    forall representation rule.
    UnifyingRuleWithRepresentation representation rule =>
    SideCondition RewritingVariableName ->
    (representation -> rule) ->
    FinalizeApplied representation ->
    Finalizer representation
finalizeRulesParallel
    sideCondition
    toRule
    finalizeApplied
    initialVariables
    initial
    unifiedRules =
        do
            results <-
                traverse
                    ( finalizeRule
                        sideCondition
                        toRule
                        finalizeApplied
                        initialVariables
                        initial
                    )
                    unifiedRules
                    & fmap fold
            let unifications = MultiOr.make (Conditional.withoutTerm <$> unifiedRules)
                remainderPredicate = Remainder.remainder' unifications
            -- FIXME pass the actual unified rules, rather than their count, to the debug function
            debugRewriteRulesRemainder initial (length unifiedRules) remainderPredicate
            -- evaluate the remainder predicate to make sure it is actually satisfiable
            SMT.evalPredicate
                (ErrorDecidePredicateUnknown $srcLoc Nothing)
                remainderPredicate
                Nothing
                >>= \case
                    -- remainder condition is UNSAT: we prune the remainder branch early to avoid
                    -- jumping into the pit of function evaluation in the configuration under the
                    -- contradictory condition (the unsatisfiable remainder)
                    Just False ->
                        return
                            Step.Results
                                { results = Seq.fromList results
                                , remainders = mempty
                                }
                    -- NB: the UNKNOWN case will trigger an exception in SMT.evalPredicate, which will
                    --     be caught by the top-level code in the RPC server and reported to the client
                    _ -> do
                        -- remainder condition is SAT: we are safe to explore
                        -- the remainder branch, i.e. to evaluate the functions in the configuration
                        -- with the remainder in the path condition and rewrite further
                        remainders <-
                            applyRemainder sideCondition initial (Condition.fromPredicate remainderPredicate)
                                & Logic.observeAllT
                                & fmap (fmap assertRemainderPattern >>> OrPattern.fromPatterns)
                        return
                            Step.Results
                                { results = Seq.fromList results
                                , remainders
                                }

finalizeSequence ::
    forall representation rule.
    UnifyingRuleWithRepresentation representation rule =>
    SideCondition RewritingVariableName ->
    (representation -> rule) ->
    FinalizeApplied representation ->
    Finalizer representation
finalizeSequence
    sideCondition
    toRule
    finalizeApplied
    initialVariables
    initial
    unifiedRules =
        do
            (results, remainder) <-
                State.runStateT
                    (traverse finalizeRuleSequence' unifiedRules)
                    (Conditional.withoutTerm initial)
            remainders <-
                applyRemainder sideCondition initial remainder
                    & Logic.observeAllT
                    & fmap (fmap assertRemainderPattern >>> OrPattern.fromPatterns)
            return
                Step.Results
                    { results = Seq.fromList $ fold results
                    , remainders
                    }
      where
        initialTerm = Conditional.term initial
        finalizeRuleSequence' unifiedRule = do
            remainder <- State.get
            let remainderPattern = Conditional.withCondition initialTerm remainder
            results <-
                finalizeRule
                    sideCondition
                    toRule
                    finalizeApplied
                    initialVariables
                    remainderPattern
                    unifiedRule
                    & Monad.Trans.lift
            let unification = Conditional.withoutTerm unifiedRule
                remainder' =
                    Condition.fromPredicate $
                        Remainder.remainder' $
                            MultiOr.singleton unification
            State.put (remainder `Conditional.andCondition` remainder')
            return results

applyWithFinalizer ::
    forall rule.
    Rule.UnifyingRule rule =>
    Rule.UnifyingRuleVariable rule ~ RewritingVariableName =>
    From rule SourceLocation =>
    From rule UniqueId =>
    From rule Label =>
    -- | SideCondition containing metadata
    SideCondition RewritingVariableName ->
    -- | Finalizing function
    Finalizer rule ->
    -- | Rewrite rules
    [rule] ->
    -- | Configuration being rewritten
    Pattern RewritingVariableName ->
    Simplifier (Results rule)
applyWithFinalizer sideCondition finalize rules initial = do
    results <- unifyRules sideCondition initial rules
    debugAppliedRewriteRules initial (locations <$> results)
    let initialVariables = freeVariables initial
    finalizedResults <- finalize initialVariables initial results
    debugRewriteTrace initial finalizedResults
    return finalizedResults
  where
    locations = from @_ @SourceLocation . extract
{-# INLINE applyWithFinalizer #-}

{- Wether to enable assuming definedness of the current
   configuration in @applyRewriteRulesParallel@
 -}
data EnableAssumeInitialDefined
    = DisableAssumeInitialDefined
    | EnableAssumeInitialDefined
    deriving stock (Show)

{- | Apply the given rules to the initial configuration in parallel.

See also: 'applyRewriteRule'
-}
applyRulesParallel ::
    -- | SideCondition containing metadata
    SideCondition RewritingVariableName ->
    -- | Rewrite rules
    [RulePattern RewritingVariableName] ->
    -- | Configuration being rewritten
    Pattern RewritingVariableName ->
    Simplifier (Results (RulePattern RewritingVariableName))
applyRulesParallel sideCondition rules initial =
    applyWithFinalizer
        sideCondition
        ( finalizeRulesParallel
            sideCondition
            RewriteRule
            (finalizeAppliedRule sideCondition)
        )
        rules
        initial

{- | Apply the given rewrite rules to the initial configuration in parallel.

See also: 'applyRewriteRule'
-}
applyRewriteRulesParallel ::
    -- | Rewrite rules
    [RewriteRule RewritingVariableName] ->
    -- | If set, assume that @initial@ and its every sub-term is defined,
    --   see @enumerateSubtermsNeedingCeil@ for details
    EnableAssumeInitialDefined ->
    -- | Configuration being rewritten
    Pattern RewritingVariableName ->
    Simplifier (Results (RulePattern RewritingVariableName))
applyRewriteRulesParallel
    (map getRewriteRule -> rules)
    assumeInitialDefined
    initial =
        do
            let sideCondition =
                    SideCondition.cacheSimplifiedFunctions
                        (Pattern.toTermLike initial)
            let subtermsNeedingCeil =
                    case assumeInitialDefined of
                        -- @enumerateSubtermsNeedingCei@l will compute all subterms that may need a @#Ceil@ predicate.
                        -- When falling back to Kore from Booster, everything must be defined, and these @#Ceil@s will evaluate to @#Top@.
                        -- However, it is difficult to convey this information from Booster to Kore; hence we conjure it up here.
                        EnableAssumeInitialDefined -> enumerateSubtermsNeedingCeil sideCondition (Pattern.toTermLike initial)
                        DisableAssumeInitialDefined -> mempty
                sideConditionWithDefinedSubterms = SideCondition.addTermsAsDefined subtermsNeedingCeil sideCondition
            results <- applyRulesParallel sideConditionWithDefinedSubterms rules initial
            assertFunctionLikeResults (term initial) results
            return results

{- | Apply the given rewrite rules to the initial configuration in sequence.

See also: 'applyRewriteRule'
-}
applyRulesSequence ::
    -- | SideCondition containing metadata
    SideCondition RewritingVariableName ->
    -- | Rewrite rules
    [RulePattern RewritingVariableName] ->
    -- | Configuration being rewritten
    Pattern RewritingVariableName ->
    Simplifier (Results (RulePattern RewritingVariableName))
applyRulesSequence sideCondition rules initial =
    applyWithFinalizer
        sideCondition
        ( finalizeSequence
            sideCondition
            RewriteRule
            (finalizeAppliedRule sideCondition)
        )
        rules
        initial

{- | Apply the given rewrite rules to the initial configuration in sequence.

See also: 'applyRewriteRulesParallel'
-}
applyRewriteRulesSequence ::
    -- | Configuration being rewritten
    Pattern RewritingVariableName ->
    -- | Rewrite rules
    [RewriteRule RewritingVariableName] ->
    Simplifier (Results (RulePattern RewritingVariableName))
applyRewriteRulesSequence
    initialConfig
    (map getRewriteRule -> rules) =
        do
            let sideCondition =
                    SideCondition.cacheSimplifiedFunctions
                        (Pattern.toTermLike initialConfig)
            results <- applyRulesSequence sideCondition rules initialConfig
            assertFunctionLikeResults (term initialConfig) results
            return results

applyClaimsSequence ::
    forall goal.
    UnifyingRuleWithRepresentation ClaimPattern goal =>
    (ClaimPattern -> goal) ->
    -- | Configuration being rewritten
    Pattern RewritingVariableName ->
    -- | Rewrite rules
    [ClaimPattern] ->
    Simplifier (Results ClaimPattern)
applyClaimsSequence mkClaim initialConfig claims = do
    let sideCondition =
            SideCondition.cacheSimplifiedFunctions
                (Pattern.toTermLike initialConfig)
    results <-
        applyWithFinalizer
            sideCondition
            ( finalizeSequence
                sideCondition
                mkClaim
                (finalizeAppliedClaim sideCondition)
            )
            claims
            initialConfig
    assertFunctionLikeResults (term initialConfig) results
    return results
