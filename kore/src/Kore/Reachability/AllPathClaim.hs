{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Reachability.AllPathClaim (
    AllPathClaim (..),
    mkAllPathClaim,
    allPathRuleToTerm,
    Rule (..),
) where

import Control.Monad (
    foldM,
 )
import Data.Binary (Binary)
import Data.Generics.Wrapped (
    _Unwrapped,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Debug
import Kore.Internal.Alias (
    Alias (aliasConstructor),
 )
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.TermLike (
    ElementVariable,
    TermLike,
    VariableName,
    getId,
    weakAlwaysFinally,
 )
import Kore.Internal.TermLike qualified as TermLike
import Kore.Reachability.Claim
import Kore.Rewrite.AxiomPattern
import Kore.Rewrite.ClaimPattern as ClaimPattern
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    mkRuleVariable,
 )
import Kore.Rewrite.Transition (
    TransitionT,
 )
import Kore.Rewrite.UnifyingRule (
    UnifyingRule (..),
 )
import Kore.Simplify.Simplify (
    Simplifier,
 )
import Kore.Syntax.Sentence qualified as Syntax
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser (
    Unparse (..),
 )
import Prelude.Kore

-- | All-Path-Claim claim pattern.
newtype AllPathClaim = AllPathClaim {getAllPathClaim :: ClaimPattern}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

mkAllPathClaim ::
    Pattern RewritingVariableName ->
    OrPattern RewritingVariableName ->
    [ElementVariable RewritingVariableName] ->
    AllPathClaim
mkAllPathClaim left right existentials =
    AllPathClaim (mkClaimPattern left right existentials)

instance Unparse AllPathClaim where
    unparse claimPattern' =
        unparse $ allPathRuleToTerm claimPattern'
    unparse2 claimPattern' =
        unparse2 $ allPathRuleToTerm claimPattern'

instance TopBottom AllPathClaim where
    isTop _ = False
    isBottom _ = False

instance From AllPathClaim Attribute.SourceLocation where
    from = Attribute.sourceLocation . attributes . getAllPathClaim

instance From AllPathClaim Attribute.Label where
    from = Attribute.label . attributes . getAllPathClaim

instance From AllPathClaim Attribute.RuleIndex where
    from = Attribute.identifier . attributes . getAllPathClaim

instance From AllPathClaim Attribute.Trusted where
    from = Attribute.trusted . attributes . getAllPathClaim

instance From AllPathClaim Attribute.UniqueId where
    from = Attribute.uniqueId . attributes . getAllPathClaim

{- | Converts an 'AllPathClaim' into its term representation.
 This is intended to be used only in unparsing situations,
 as some of the variable information related to the
 rewriting algorithm is lost.
-}
allPathRuleToTerm :: AllPathClaim -> TermLike VariableName
allPathRuleToTerm (AllPathClaim claimPattern') =
    claimPatternToTerm TermLike.WAF claimPattern'

instance UnifyingRule AllPathClaim where
    type UnifyingRuleVariable AllPathClaim = RewritingVariableName

    matchingPattern (AllPathClaim claim) = matchingPattern claim

    precondition (AllPathClaim claim) = precondition claim

    refreshRule stale (AllPathClaim claim) =
        AllPathClaim <$> refreshRule stale claim

instance From AllPathClaim (AxiomPattern VariableName) where
    from = AxiomPattern . allPathRuleToTerm

instance From AllPathClaim (AxiomPattern RewritingVariableName) where
    from =
        AxiomPattern
            . TermLike.mapVariables (pure mkRuleVariable)
            . allPathRuleToTerm

instance Claim AllPathClaim where
    newtype Rule AllPathClaim = AllPathRewriteRule
        {unRuleAllPath :: RewriteRule RewritingVariableName}
        deriving stock (Eq, Ord, Show)
        deriving stock (GHC.Generic)
        deriving anyclass (Binary)
        deriving anyclass (NFData)
        deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
        deriving anyclass (Debug, Diff)
        deriving newtype (Unparse)

    simplify = simplify' _Unwrapped
    checkImplication = checkImplication' _Unwrapped
    applyClaims claims = deriveSeqClaim _Unwrapped AllPathClaim claims

    applyAxioms axiomss = \claim ->
        foldM applyAxioms1 (ApplyRemainder claim) axiomss
      where
        applyAxioms1 applied axioms
            | Just claim <- retractApplyRemainder applied =
                deriveParAxiomAllPath axioms claim
                    >>= simplifyRemainder
            | otherwise =
                pure applied

        simplifyRemainder applied =
            case applied of
                ApplyRemainder claim -> ApplyRemainder <$> simplify claim
                _ -> return applied

instance From (Rule AllPathClaim) Attribute.PriorityAttributes where
    from = from @(RewriteRule _) . unRuleAllPath

instance ClaimExtractor AllPathClaim where
    extractClaim (attributes, sentence) =
        case termLike of
            TermLike.Implies_
                _
                (TermLike.And_ _ requires lhs)
                (TermLike.ApplyAlias_ alias [rhs])
                    | aliasId == weakAlwaysFinally -> do
                        let rhs' = TermLike.mapVariables (pure mkRuleVariable) rhs
                            attributes' =
                                Attribute.mapAxiomVariables
                                    (pure mkRuleVariable)
                                    attributes
                            (right', existentials') =
                                ClaimPattern.termToExistentials rhs'
                        pure $
                            AllPathClaim $
                                ClaimPattern.refreshExistentials
                                    ClaimPattern
                                        { ClaimPattern.left =
                                            Pattern.fromTermAndPredicate
                                                lhs
                                                (Predicate.wrapPredicate requires)
                                                & Pattern.mapVariables (pure mkRuleVariable)
                                        , ClaimPattern.right = parseRightHandSide right'
                                        , ClaimPattern.existentials = existentials'
                                        , ClaimPattern.attributes = attributes'
                                        }
                  where
                    aliasId = (getId . aliasConstructor) alias
            _ -> Nothing
      where
        termLike =
            (Syntax.sentenceAxiomPattern . Syntax.getSentenceClaim) sentence

deriveParAxiomAllPath ::
    [Rule AllPathClaim] ->
    AllPathClaim ->
    TransitionT
        (AppliedRule AllPathClaim)
        Simplifier
        (ApplyResult AllPathClaim)
deriveParAxiomAllPath rules =
    derivePar' _Unwrapped AllPathRewriteRule rewrites
  where
    rewrites = unRuleAllPath <$> rules
