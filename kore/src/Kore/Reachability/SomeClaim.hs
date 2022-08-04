{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Reachability.SomeClaim (
    SomeClaim (..),
    mkSomeClaimOnePath,
    mkSomeClaimAllPath,
    makeTrusted,
    getConfiguration,
    getDestination,
    lensClaimPattern,

    -- * Re-exports
    Claim (..),
    ClaimExtractor (..),
    Rule (..),
    AllPathClaim (..),
    OnePathClaim (..),
) where

import Control.Lens qualified as Lens
import Data.Coerce (
    coerce,
 )
import Data.Default qualified as Default
import Data.Generics.Product
import Data.Generics.Wrapped
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Debug
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.TermLike (
    ElementVariable,
    VariableName,
 )
import Kore.Reachability.AllPathClaim
import Kore.Reachability.Claim
import Kore.Reachability.OnePathClaim
import Kore.Rewrite.AxiomPattern
import Kore.Rewrite.ClaimPattern (
    ClaimPattern,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
 )
import Kore.Rewrite.Transition (
    TransitionT,
 )
import Kore.Rewrite.Transition qualified as Transition
import Kore.Syntax.Definition qualified as Syntax
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser (
    Unparse (..),
 )
import Kore.Verified qualified as Verified
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

{- | Some claim in reachability logic.

@SomeClaim@ may be a 'OnePathClaim' or an 'AllPathClaim'.
-}
data SomeClaim
    = OnePath !OnePathClaim
    | AllPath !AllPathClaim
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (NFData, Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

mkSomeClaimAllPath ::
    Pattern RewritingVariableName ->
    OrPattern RewritingVariableName ->
    [ElementVariable RewritingVariableName] ->
    SomeClaim
mkSomeClaimAllPath left right existentials =
    AllPath (mkAllPathClaim left right existentials)

mkSomeClaimOnePath ::
    Pattern RewritingVariableName ->
    OrPattern RewritingVariableName ->
    [ElementVariable RewritingVariableName] ->
    SomeClaim
mkSomeClaimOnePath left right existentials =
    OnePath (mkOnePathClaim left right existentials)

instance Unparse SomeClaim where
    unparse (OnePath rule) = unparse rule
    unparse (AllPath rule) = unparse rule
    unparse2 (AllPath rule) = unparse2 rule
    unparse2 (OnePath rule) = unparse2 rule

instance TopBottom SomeClaim where
    isTop _ = False
    isBottom _ = False

instance Pretty SomeClaim where
    pretty (OnePath (OnePathClaim rule)) =
        Pretty.vsep ["One-Path reachability rule:", Pretty.pretty rule]
    pretty (AllPath (AllPathClaim rule)) =
        Pretty.vsep ["All-Path reachability rule:", Pretty.pretty rule]

instance From SomeClaim Attribute.SourceLocation where
    from (OnePath onePathRule) = from onePathRule
    from (AllPath allPathRule) = from allPathRule

instance From SomeClaim Attribute.Label where
    from (OnePath onePathRule) = from onePathRule
    from (AllPath allPathRule) = from allPathRule

instance From SomeClaim Attribute.RuleIndex where
    from (OnePath onePathRule) = from onePathRule
    from (AllPath allPathRule) = from allPathRule

instance From SomeClaim Attribute.Trusted where
    from (OnePath onePathRule) = from onePathRule
    from (AllPath allPathRule) = from allPathRule

instance From SomeClaim (AxiomPattern VariableName) where
    from (OnePath rule) = from rule
    from (AllPath rule) = from rule

instance From SomeClaim Verified.Sentence where
    from claim =
        Syntax.SentenceClaimSentence $
            Syntax.SentenceClaim
                Syntax.SentenceAxiom
                    { sentenceAxiomParameters = []
                    , sentenceAxiomPattern
                    , sentenceAxiomAttributes = Default.def
                    }
      where
        AxiomPattern sentenceAxiomPattern = from claim

getConfiguration :: SomeClaim -> Pattern RewritingVariableName
getConfiguration = Lens.view (lensClaimPattern . field @"left")

getDestination :: SomeClaim -> OrPattern RewritingVariableName
getDestination = Lens.view (lensClaimPattern . field @"right")

lensClaimPattern ::
    Functor f =>
    (ClaimPattern -> f ClaimPattern) ->
    SomeClaim ->
    f SomeClaim
lensClaimPattern =
    Lens.lens
        ( \case
            OnePath onePathRule ->
                Lens.view _Unwrapped onePathRule
            AllPath allPathRule ->
                Lens.view _Unwrapped allPathRule
        )
        ( \case
            OnePath onePathRule -> \attrs ->
                onePathRule
                    & Lens.set _Unwrapped attrs
                    & OnePath
            AllPath allPathRule -> \attrs ->
                allPathRule
                    & Lens.set _Unwrapped attrs
                    & AllPath
        )

makeTrusted :: SomeClaim -> SomeClaim
makeTrusted =
    Lens.set
        ( lensClaimPattern
            . field @"attributes"
            . field @"trusted"
        )
        (Attribute.Trusted True)

instance Claim SomeClaim where
    newtype Rule SomeClaim = ReachabilityRewriteRule
        {unReachabilityRewriteRule :: RewriteRule RewritingVariableName}
        deriving stock (Eq, Ord, Show)
        deriving stock (GHC.Generic)
        deriving anyclass (NFData)
        deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
        deriving anyclass (Debug, Diff)
        deriving newtype (Unparse)

    simplify (AllPath claim) = allPathTransition $ AllPath <$> simplify claim
    simplify (OnePath claim) = onePathTransition $ OnePath <$> simplify claim

    checkImplication (AllPath claim) = fmap AllPath <$> checkImplication claim
    checkImplication (OnePath claim) = fmap OnePath <$> checkImplication claim

    applyClaims claims (AllPath claim) =
        applyClaims (mapMaybe maybeAllPath claims) claim
            & fmap (fmap AllPath)
            & allPathTransition
    applyClaims claims (OnePath claim) =
        applyClaims (mapMaybe maybeOnePath claims) claim
            & fmap (fmap OnePath)
            & onePathTransition

    applyAxioms axiomGroups (AllPath claim) =
        applyAxioms (coerce axiomGroups) claim
            & fmap (fmap AllPath)
            & allPathTransition
    applyAxioms axiomGroups (OnePath claim) =
        applyAxioms (coerce axiomGroups) claim
            & fmap (fmap OnePath)
            & onePathTransition

instance From (Rule SomeClaim) Attribute.PriorityAttributes where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance From (Rule SomeClaim) Attribute.SourceLocation where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance From (Rule SomeClaim) Attribute.Label where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance From (Rule SomeClaim) Attribute.RuleIndex where
    from = from @(RewriteRule _) . unReachabilityRewriteRule

instance ClaimExtractor SomeClaim where
    extractClaim input =
        (OnePath <$> extractClaim input) <|> (AllPath <$> extractClaim input)

allPathTransition ::
    Monad m =>
    TransitionT (AppliedRule AllPathClaim) m a ->
    TransitionT (AppliedRule SomeClaim) m a
allPathTransition = Transition.mapRules ruleAllPathToRuleReachability

onePathTransition ::
    Monad m =>
    TransitionT (AppliedRule OnePathClaim) m a ->
    TransitionT (AppliedRule SomeClaim) m a
onePathTransition = Transition.mapRules ruleOnePathToRuleReachability

maybeOnePath :: SomeClaim -> Maybe OnePathClaim
maybeOnePath (OnePath rule) = Just rule
maybeOnePath _ = Nothing

maybeAllPath :: SomeClaim -> Maybe AllPathClaim
maybeAllPath (AllPath rule) = Just rule
maybeAllPath _ = Nothing

ruleAllPathToRuleReachability ::
    AppliedRule AllPathClaim ->
    AppliedRule SomeClaim
ruleAllPathToRuleReachability (AppliedAxiom (AllPathRewriteRule rewriteRule)) =
    AppliedAxiom (ReachabilityRewriteRule rewriteRule)
ruleAllPathToRuleReachability (AppliedClaim allPathRule) =
    AppliedClaim (AllPath allPathRule)

ruleOnePathToRuleReachability ::
    AppliedRule OnePathClaim ->
    AppliedRule SomeClaim
ruleOnePathToRuleReachability (AppliedAxiom (OnePathRewriteRule rewriteRule)) =
    AppliedAxiom (ReachabilityRewriteRule rewriteRule)
ruleOnePathToRuleReachability (AppliedClaim onePathRule) =
    AppliedClaim (OnePath onePathRule)
