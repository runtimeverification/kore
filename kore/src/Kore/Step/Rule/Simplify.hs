{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.Step.Rule.Simplify
    ( SimplifyRuleLHS (..)
    ) where

import Prelude.Kore

import Control.Lens
    ( Lens'
    )
import qualified Control.Lens as Lens
import Control.Monad
    ( (>=>)
    )

import Branch
    ( BranchT
    )
import qualified Branch
import Kore.Internal.Conditional
    ( Conditional (Conditional)
    )
import qualified Kore.Internal.Conditional as Conditional.DoNotUse
    ( Conditional (..)
    )
import Kore.Internal.MultiAnd
    ( MultiAnd
    )
import qualified Kore.Internal.MultiAnd as MultiAnd
    ( make
    )
import qualified Kore.Internal.MultiOr as MultiOr
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( Predicate
    , makeAndPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
    ( coerceSort
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( top
    )
import Kore.Internal.TermLike
    ( termLikeSort
    )
import Kore.Step.RulePattern
    ( AllPathRule (..)
    , OnePathRule (..)
    , ReachabilityRule (..)
    , RewriteRule (..)
    , RulePattern (RulePattern)
    )
import qualified Kore.Step.RulePattern as RulePattern
    ( RulePattern (..)
    , applySubstitution
    )
import Kore.Step.Simplification.OrPattern
    ( simplifyConditionsWithSmt
    )
import qualified Kore.Step.Simplification.Pattern as Pattern
import Kore.Step.Simplification.Simplify
    ( InternalVariable
    , MonadSimplify
    )
import Kore.Syntax.Variable
    ( Variable
    )

-- | Simplifies the left-hand-side of a rule/claim
class SimplifyRuleLHS rule where
    simplifyRuleLhs
        :: forall simplifier
        .  MonadSimplify simplifier
        => rule
        -> simplifier (MultiAnd rule)

instance InternalVariable variable => SimplifyRuleLHS (RulePattern variable)
  where
    simplifyRuleLhs rule@(RulePattern _ _ _ _ _) = do
        let lhsWithPredicate = Pattern.fromTermLike left
        simplifiedTerms <-
            Pattern.simplifyTopConfiguration lhsWithPredicate
        fullySimplified <-
            simplifyConditionsWithSmt
                SideCondition.top
                simplifiedTerms
        let rules =
                map (setRuleLeft rule) (MultiOr.extractPatterns fullySimplified)
        return (MultiAnd.make rules)
      where
        RulePattern {left} = rule

        setRuleLeft
            :: RulePattern variable
            -> Pattern variable
            -> RulePattern variable
        setRuleLeft
            rulePattern@RulePattern {requires = requires'}
            Conditional {term, predicate, substitution}
          =
            RulePattern.applySubstitution
                substitution
                rulePattern
                    { RulePattern.left = term
                    , RulePattern.requires =
                        Predicate.coerceSort (termLikeSort term)
                        $ makeAndPredicate predicate requires'
                    }

instance SimplifyRuleLHS (RewriteRule Variable) where
    simplifyRuleLhs =
        fmap (fmap RewriteRule) . simplifyRuleLhs . getRewriteRule

instance SimplifyRuleLHS (OnePathRule Variable) where
    simplifyRuleLhs =
        fmap (fmap OnePathRule) . simplifyClaimRule . getOnePathRule

instance SimplifyRuleLHS (AllPathRule Variable) where
    simplifyRuleLhs =
        fmap (fmap AllPathRule) . simplifyClaimRule . getAllPathRule

instance SimplifyRuleLHS (ReachabilityRule Variable) where
    simplifyRuleLhs (OnePath rule) =
        (fmap . fmap) OnePath $ simplifyRuleLhs rule
    simplifyRuleLhs (AllPath rule) =
        (fmap . fmap) AllPath $ simplifyRuleLhs rule

simplifyClaimRule
    :: forall simplifier variable
    .  MonadSimplify simplifier
    => InternalVariable variable
    => RulePattern variable
    -> simplifier (MultiAnd (RulePattern variable))
simplifyClaimRule =
    fmap MultiAnd.make . Branch.gather . Lens.traverseOf leftPattern worker
  where
    worker, simplify, simplifyWithSolver
        :: Pattern variable
        -> BranchT simplifier (Pattern variable)
    worker =
        (return . Pattern.requireDefined)
        >=> simplify
        >=> simplifyWithSolver
    simplify =
        Pattern.simplifyTopConfiguration
        >=> Branch.scatter
    simplifyWithSolver =
        (return . OrPattern.fromPattern)
        >=> simplifyConditionsWithSmt SideCondition.top
        >=> Branch.scatter

{- | A 'Lens\'' to view the left-hand side of a 'RulePattern' as a 'Pattern'.
 -}
leftPattern
    :: InternalVariable variable
    => Lens' (RulePattern variable) (Pattern variable)
leftPattern =
    Lens.lens get set
  where
    get RulePattern { left, requires } =
        Pattern.withCondition left $ from @(Predicate _) requires
    set rule@(RulePattern _ _ _ _ _) pattern' =
        RulePattern.applySubstitution
            (Pattern.substitution pattern')
            rule
                { RulePattern.left = Pattern.term pattern'
                , RulePattern.requires =
                    Predicate.coerceSort
                        (termLikeSort $ Pattern.term pattern')
                        (Pattern.predicate pattern')
                }
