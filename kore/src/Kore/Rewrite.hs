{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause

Strategy-based interface to rule application (step-wise execution).
-}
module Kore.Rewrite (
    ExecutionMode (..),
    ProgramState (..),
    RuleInfo (..),
    Prim (..),
    TransitionRule,
    executionStrategy,
    extractProgramState,
    transitionRule,
    groupRewritesByPriority,
    limitedExecutionStrategy,

    -- * Re-exports
    Natural,
) where

import Control.Monad (
    foldM,
 )
import Control.Monad.State (get, modify)
import Data.Bifunctor
import Data.Limit (
    Limit (..),
 )
import Data.Limit qualified as Limit
import Data.List.Extra (
    groupSortOn,
 )
import Data.Stream.Infinite (
    Stream,
 )
import Data.Stream.Infinite qualified as Stream
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.UniqueId (UniqueId)
import Kore.Debug
import Kore.Internal.Pattern (
    Conditional (predicate, substitution),
    Pattern,
 )
import Kore.Internal.Predicate (Predicate)
import Kore.Internal.Substitution (Substitution)
import Kore.Log.DebugContext (inContext)
import Kore.Log.DecidePredicateUnknown (srcLoc)
import Kore.Rewrite.Result qualified as Result
import Kore.Rewrite.RewriteStep qualified as Step
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.RulePattern (
    RewriteRule (..),
    RulePattern,
 )
import Kore.Rewrite.SMT.Evaluator qualified as SMT.Evaluator (
    filterMultiOr,
 )
import Kore.Rewrite.Strategy hiding (
    transitionRule,
 )
import Kore.Rewrite.Transition qualified as Strategy (
    TransitionT,
 )
import Kore.Simplify.Pattern qualified as Pattern (
    simplifyTopConfiguration,
 )
import Kore.Simplify.Simplify as Simplifier
import Kore.TopBottom (
    TopBottom,
    isBottom,
 )
import Kore.Unparser (
    Unparse (..),
 )
import Numeric.Natural (
    Natural,
 )
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

data RuleInfo a = RuleInfo
    { rulePredicate :: Predicate a
    , ruleSubstitution :: Substitution a
    , ruleId :: UniqueId
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | The program's state during symbolic execution.
data ProgramState b a
    = -- | The beginning of an execution step.
      Start !a
    | -- | The configuration was rewritten after applying
      -- the rewrite rules.
      Rewritten !b !a
    | -- | The configuration is a remainder resulting
      -- from rewrite rule application.
      Remaining !a
    | -- | The execution step yields no children
      Bottom
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Bifunctor ProgramState where
    bimap f g = \case
        Start a -> Start $ g a
        Rewritten b a -> Rewritten (f b) (g a)
        Remaining a -> Remaining $ g a
        Bottom -> Bottom

instance Unparse a => Pretty (ProgramState b a) where
    pretty (Start a) =
        Pretty.vsep
            [ "start:"
            , Pretty.indent 4 $ unparse a
            ]
    pretty (Rewritten _ a) =
        Pretty.vsep
            [ "rewritten:"
            , Pretty.indent 4 $ unparse a
            ]
    pretty (Remaining a) =
        Pretty.vsep
            [ "remaining:"
            , Pretty.indent 4 $ unparse a
            ]
    pretty Bottom = "\\bottom"

extractProgramState :: ProgramState b a -> (Maybe a, Maybe b)
extractProgramState (Rewritten b a) = (Just a, Just b)
extractProgramState (Remaining a) = (Just a, Nothing)
extractProgramState (Start a) = (Just a, Nothing)
extractProgramState Bottom = (Nothing, Nothing)

retractRemaining :: ProgramState b a -> Maybe a
retractRemaining (Remaining a) = Just a
retractRemaining _ = Nothing

-- | The sequence of transitions for the symbolic execution strategy.
executionStrategy :: Stream (Step Prim)
executionStrategy =
    step1 Stream.:> Stream.iterate id stepN
  where
    step1 =
        [ Begin
        , Simplify
        , Rewrite
        , Simplify
        ]
    stepN =
        [ Begin
        , Rewrite
        , Simplify
        ]

{- | The sequence of transitions under the specified depth limit.

See also: 'executionStrategy'
-}
limitedExecutionStrategy :: Limit Natural -> [Step Prim]
limitedExecutionStrategy depthLimit =
    Limit.takeWithin depthLimit (toList executionStrategy)

{- The primitive transitions for the symbolic execution strategy.
-}
data Prim
    = Begin
    | Simplify
    | Rewrite
    deriving stock (Eq, Show)

{- The two modes of symbolic execution. Each mode determines the way
    rewrite rules are applied during a rewrite step.
-}
data ExecutionMode = All | Any
    deriving stock (Show)

-- | @TransitionRule@ is the general type of transition rules over 'Prim'.
type TransitionRule monad rule state =
    Prim -> state -> Strategy.TransitionT rule monad state

-- | Transition rule for primitive strategies in 'Prim'.
transitionRule ::
    [[RewriteRule RewritingVariableName]] ->
    Step.EnableAssumeInitialDefined ->
    ExecutionMode ->
    TransitionRule
        Simplifier
        (RewriteRule RewritingVariableName, Seq SimplifierTrace)
        ( ProgramState
            (RuleInfo RewritingVariableName)
            (Pattern RewritingVariableName)
        )
transitionRule rewriteGroups assumeInitialDefined = transitionRuleWorker
  where
    transitionRuleWorker _ Simplify Bottom = pure Bottom
    transitionRuleWorker _ _ Bottom = empty
    transitionRuleWorker _ Begin (Rewritten _ a) = pure $ Start a
    transitionRuleWorker _ Begin (Remaining _) = empty
    transitionRuleWorker _ Begin state@(Start _) = pure state
    transitionRuleWorker _ Simplify (Rewritten x patt) =
        transitionSimplify (Rewritten x) patt
    transitionRuleWorker _ Simplify (Remaining patt) =
        transitionSimplify Remaining patt
    transitionRuleWorker _ Simplify (Start patt) =
        transitionSimplify Start patt
    transitionRuleWorker mode Rewrite (Remaining patt) =
        transitionRewrite mode patt
    transitionRuleWorker mode Rewrite (Start patt) =
        transitionRewrite mode patt
    transitionRuleWorker _ Rewrite state@(Rewritten _ _) =
        pure state

    transitionSimplify prim config = do
        configs <- lift $ Pattern.simplifyTopConfiguration config
        filteredConfigs <- liftSimplifier $ SMT.Evaluator.filterMultiOr $srcLoc configs
        if isBottom filteredConfigs
            then pure Bottom
            else prim <$> asum (pure <$> toList filteredConfigs)

    transitionRewrite All patt = inContext "transitionAllRewrite" $ transitionAllRewrite patt
    transitionRewrite Any patt = transitionAnyRewrite patt

    transitionAllRewrite config =
        foldM transitionRewrite' (Remaining config) rewriteGroups
      where
        transitionRewrite' applied rewrites
            | Just config' <- retractRemaining applied =
                Step.applyRewriteRulesParallel rewrites assumeInitialDefined config'
                    & lift
                    >>= deriveResults
                    >>= simplifyRemainder
            | otherwise = pure applied
        simplifyRemainder (Remaining p) = transitionSimplify Remaining p
        simplifyRemainder p = return p

    transitionAnyRewrite config = do
        let rules = concat rewriteGroups
        results <- Step.applyRewriteRulesSequence config rules & lift
        deriveResults results

deriveResults ::
    TopBottom a =>
    Result.Results
        ( Conditional
            var
            (RulePattern var)
        )
        a ->
    TransitionT
        (RewriteRule var, Seq SimplifierTrace)
        Simplifier
        (ProgramState (RuleInfo var) a)
deriveResults Result.Results{results, remainders} =
    if (null results || all (\Result.Result{result} -> isBottom result) results) && null remainders
        then pure Bottom
        else addResults results <|> addRemainders remainders
  where
    addResults results' = asum (addResult <$> results')
    addResult Result.Result{appliedRule, result}
        | isBottom result = empty
        | otherwise = do
            (_, simplifyRules :: Seq SimplifierTrace) <- lift get
            lift $ modify $ \(cache, _rules) -> (cache, mempty)
            let rulePattern = extract appliedRule
            let
                ruleLabel :: Attribute.Label = from rulePattern
                ruleUniqueId :: UniqueId = from rulePattern
            addRule (RewriteRule rulePattern, simplifyRules)
            pure $
                Rewritten
                    ( RuleInfo
                        (predicate appliedRule)
                        (substitution appliedRule)
                        (uniqueIdWithFallback ruleUniqueId ruleLabel)
                    )
                    result
    addRemainders remainders' =
        asum (pure . Remaining <$> toList remainders')

    -- Some rewrite rules are dynamically generated and injected into
    -- a running server using the RPC "add-module" endpoint.
    -- These rules, for historical reasons, are not given unique ids. Hopefully they will be in future.
    -- Until then, we use the rule label, if available, in place of the ID.
    uniqueIdWithFallback :: UniqueId -> Attribute.Label -> UniqueId
    uniqueIdWithFallback (Attribute.UniqueId mbUniqueId) (Attribute.Label mbLabel) = Attribute.UniqueId . Just $
        case mbUniqueId of
            Just uid -> uid
            Nothing -> fromMaybe "UNKNOWN" mbLabel

groupRewritesByPriority ::
    [RewriteRule variable] -> [[RewriteRule variable]]
groupRewritesByPriority rewriteRules =
    groupSortOn Attribute.getPriorityOfAxiom rewriteRules
