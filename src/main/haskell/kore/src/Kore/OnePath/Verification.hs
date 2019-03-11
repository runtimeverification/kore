{-|
Module      : Kore.OnePath.Verification
Description : One-path verification
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com

This should be imported qualified.
-}

module Kore.OnePath.Verification
    ( Axiom (..)
    , Claim (..)
    , isTrusted
    , defaultStrategy
    , verify
    , verifyClaimStep
    ) where

import Control.Monad.Trans.Except
       ( ExceptT, throwE )
import Numeric.Natural
       ( Natural )

import qualified Control.Monad.Trans as Monad.Trans
import           Data.Coerce
                 ( coerce )
import qualified Data.Graph.Inductive.Graph as Graph
import           Data.Limit
                 ( Limit )
import qualified Data.Limit as Limit
import           Data.Profunctor
                 ( dimap )
import           Kore.AST.Common
                 ( Variable )
import           Kore.AST.MetaOrObject
                 ( MetaOrObject (..) )
import qualified Kore.Attribute.Axiom as Attribute
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.OnePath.Step
                 ( Prim, StrategyPattern, onePathFirstStep,
                 onePathFollowupStep )
import qualified Kore.OnePath.Step as StrategyPattern
                 ( StrategyPattern (..) )
import qualified Kore.OnePath.Step as OnePath
                 ( transitionRule )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.AxiomPatterns
                 ( RewriteRule (RewriteRule), RulePattern (RulePattern) )
import           Kore.Step.AxiomPatterns as RulePattern
                 ( RulePattern (..) )
import           Kore.Step.BaseStep
                 ( StepProof )
import           Kore.Step.Pattern
                 ( CommonStepPattern )
import           Kore.Step.Representation.ExpandedPattern
                 ( CommonExpandedPattern, Predicated (Predicated) )
import           Kore.Step.Representation.ExpandedPattern as ExpandedPattern
                 ( fromPurePattern )
import           Kore.Step.Representation.ExpandedPattern as Predicated
                 ( Predicated (..) )
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier, Simplifier,
                 StepPatternSimplifier )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import           Kore.Step.Strategy
                 ( executionHistoryStep )
import           Kore.Step.Strategy
                 ( Strategy, pickFinal, runStrategy )
import           Kore.Step.Strategy
                 ( ExecutionGraph (..) )

{- | Wrapper for a rewrite rule that should be used as a claim.
-}
data Claim level = Claim
    { rule :: !(RewriteRule level Variable)
    , attributes :: !Attribute.Axiom
    }

-- | Is the 'Claim' trusted?
isTrusted :: Claim level -> Bool
isTrusted Claim { attributes = Attribute.Axiom { trusted } }=
    Attribute.isTrusted trusted

{- | Wrapper for a rewrite rule that should be used as an axiom.
-}
newtype Axiom level = Axiom (RewriteRule level Variable)

{- | Verifies a set of claims. When it verifies a certain claim, after the
first step, it also uses the claims as axioms (i.e. it does coinductive proofs).

If the verification fails, returns an error containing a pattern that could
not be rewritten (either because no axiom could be applied or because we
didn't manage to verify a claim within the its maximum number of steps.

If the verification succeeds, it returns ().
-}
verify
    :: MetaOrObject level
    => MetadataTools level StepperAttributes
    -> StepPatternSimplifier level
    -- ^ Simplifies normal patterns through, e.g., function evaluation
    -> PredicateSubstitutionSimplifier level
    -- ^ Simplifies predicates
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    ->  (  CommonStepPattern level
        -> [Strategy
            (Prim
                (CommonExpandedPattern level)
                (RewriteRule level Variable)
            )
           ]
        )
    -- ^ Creates a one-step strategy from a target pattern. See
    -- 'defaultStrategy'.
    -> [(RewriteRule level Variable, Limit Natural)]
    -- ^ List of claims, together with a maximum number of verification steps
    -- for each.
    -> ExceptT
        (CommonExpandedPattern level)
        Simplifier
        ()
verify
    metadataTools
    simplifier
    substitutionSimplifier
    axiomIdToSimplifier
    strategyBuilder
  =
    mapM_
        (verifyClaim
            metadataTools
            simplifier
            substitutionSimplifier
            axiomIdToSimplifier
            strategyBuilder
        )

{- | Default implementation for a one-path strategy. You can apply it to the
first two arguments and pass the resulting function to 'verify'.

Things to note when implementing your own:

1. The first step does not use the reachability claims

2. You can return an infinite list.
-}
defaultStrategy
    :: forall level
    .   (MetaOrObject level)
    => [Claim level]
    -- The claims that we want to prove
    -> [Axiom level]
    -> CommonStepPattern level
    -> [Strategy
        (Prim
            (CommonExpandedPattern level)
            (RewriteRule level Variable)
        )
       ]
defaultStrategy
    claims
    axioms
    target
  =
    onePathFirstStep expandedTarget rewrites
    : repeat
        (onePathFollowupStep
            expandedTarget
            coinductiveRewrites
            rewrites
        )
  where
    rewrites :: [RewriteRule level Variable]
    rewrites = map unwrap axioms
      where
        unwrap (Axiom a) = a
    coinductiveRewrites :: [RewriteRule level Variable]
    coinductiveRewrites = map rule claims
    expandedTarget :: CommonExpandedPattern level
    expandedTarget = ExpandedPattern.fromPurePattern target

verifyClaim
    :: forall level . (MetaOrObject level)
    => MetadataTools level StepperAttributes
    -> StepPatternSimplifier level
    -> PredicateSubstitutionSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from symbol IDs to defined functions
    ->  (  CommonStepPattern level
        -> [Strategy
            (Prim
                (CommonExpandedPattern level)
                (RewriteRule level Variable)
            )
           ]
        )
    -> (RewriteRule level Variable, Limit Natural)
    -> ExceptT
        (CommonExpandedPattern level)
        Simplifier
        ()
verifyClaim
    metadataTools
    simplifier
    substitutionSimplifier
    axiomIdToSimplifier
    strategyBuilder
    ((RewriteRule RulePattern {left, right, requires}), stepLimit)
  = do
    let
        strategy =
            Limit.takeWithin
                stepLimit
                (strategyBuilder right)
        startPattern :: StrategyPattern (CommonExpandedPattern level)
        startPattern =
            StrategyPattern.RewritePattern
                Predicated
                    {term = left, predicate = requires, substitution = mempty}
    executionGraph <- Monad.Trans.lift $ runStrategy
        transitionRule'
        strategy
        ( startPattern, mempty )
    let
        finalNodes = pickFinal executionGraph
        nonBottomNodes = filter notBottom (map fst finalNodes)
        notBottom StrategyPattern.Bottom = False
        notBottom _ = True
    case nonBottomNodes of
        [] -> return ()
        StrategyPattern.RewritePattern p : _ -> throwE p
        StrategyPattern.Stuck p : _ -> throwE p
        StrategyPattern.Bottom : _ -> error "Unexpected bottom pattern."
  where
    transitionRule'
        :: Prim (CommonExpandedPattern level) (RewriteRule level Variable)
        -> (StrategyPattern (CommonExpandedPattern level), StepProof level Variable)
        -> Simplifier [(StrategyPattern (CommonExpandedPattern level), StepProof level Variable)]
    transitionRule' =
        OnePath.transitionRule
            metadataTools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier

-- | TODO: Docs.
type Configuration level = StrategyPattern (CommonExpandedPattern level)


-- | Attempts to perform a single proof step, starting at the configuration
-- in the execution graph designated by the provided node. Re-constructs the
-- execution graph by inserting this step.
verifyClaimStep
    :: forall level
    .  MetaOrObject level
    => MetadataTools level StepperAttributes
    -> StepPatternSimplifier level
    -> PredicateSubstitutionSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> Claim level
    -- ^ claim that is being proven
    -> [Claim level]
    -- ^ list of claims in the spec module
    -> [Axiom level]
    -- ^ list of axioms in the main module
    -> ExecutionGraph (Configuration level)
    -- ^ current execution graph
    -> Graph.Node
    -- ^ selected node in the graph
    -> Simplifier (ExecutionGraph (Configuration level))
verifyClaimStep
    tools
    simplifier
    predicateSimplifier
    axiomIdToSimplifier
    target
    claims
    axioms
    eg@ExecutionGraph { root }
    node
  = executionHistoryStep
        transitionRule'
        strategy'
        eg
        node
  where
    transitionRule'
        :: Prim (CommonExpandedPattern level) (RewriteRule level Variable)
        -> StrategyPattern (CommonExpandedPattern level)
        -> Simplifier [StrategyPattern (CommonExpandedPattern level)]
    transitionRule' =
        stripProof
            mempty
            $ OnePath.transitionRule
                tools
                predicateSimplifier
                simplifier
                axiomIdToSimplifier

    strategy' :: Strategy (Prim (CommonExpandedPattern level) (RewriteRule level Variable))
    strategy'
        | isRoot = onePathFirstStep targetPattern rewrites
        | otherwise = onePathFollowupStep targetPattern (rule <$> claims) rewrites

    rewrites :: [RewriteRule level Variable]
    rewrites = coerce <$> axioms

    targetPattern :: CommonExpandedPattern level
    targetPattern =
        ExpandedPattern.fromPurePattern
            . right
            . coerce
            . rule
            $ target

    isRoot :: Bool
    isRoot = node == root

    stripProof
        :: forall prim strategy f g proof
        .  (Functor f, Functor g)
        => proof
        -> (prim -> (strategy, proof) -> f (g (strategy, proof)))
        -> prim -> strategy -> f (g strategy)
    stripProof defaultProof fn prim =
        dimap
            (\a -> (a, defaultProof))
            ((fmap . fmap) fst)
            $ fn prim
