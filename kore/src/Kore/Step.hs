{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

Strategy-based interface to rule application (step-wise execution).

 -}

module Kore.Step
    ( -- * Primitive strategies
      Prim (..)
    , ExecutionState (..)
    , ExecutionPrim (..)
    , ExecutionTransitionRule
    , executionStrategy
    , extractExecutionState
    , rewrite
    , simplify
    , rewriteStep
    , priorityAllStrategy
    , priorityAnyStrategy
    , TransitionRule
    , transitionRule
    , transitionRuleSearch
      -- * Re-exports
    , RulePattern
    , Natural
    , Strategy
    , pickLongest
    , pickFinal
    , runStrategy
    ) where

-- import Kore.Unparser
--     ( unparseToString
--     )
import Prelude.Kore

import Control.Monad
    ( foldM
    )
import Data.List.Extra
    ( groupSortOn
    , sortOn
    )
import Data.Stream.Infinite
    ( Stream
    )
import qualified Data.Stream.Infinite as Stream
import Numeric.Natural
    ( Natural
    )

import qualified Kore.Attribute.Axiom as Attribute
import Kore.Internal.Pattern
    ( Pattern
    )
import Kore.Rewriting.RewritingVariable
import qualified Kore.Step.Result as Result
import qualified Kore.Step.RewriteStep as Step
import Kore.Step.RulePattern
    ( RewriteRule (..)
    , RulePattern
    )
import qualified Kore.Step.Simplification.Pattern as Pattern
    ( simplifyTopConfiguration
    )
import Kore.Step.Simplification.Simplify as Simplifier

import qualified Kore.Step.SMT.Evaluator as SMT.Evaluator
    ( filterMultiOr
    )
import qualified Kore.Step.Step as Step
import Kore.Step.Strategy hiding
    ( transitionRule
    )
import qualified Kore.Step.Strategy as Strategy
import qualified Kore.Step.Transition as Transition
import qualified Kore.Unification.Procedure as Unification

{- TODO: docs
-}
data ExecutionState a = StartExec !a | Rewritten !a | Remaining !a
    deriving (Show, Functor)

extractExecutionState :: ExecutionState a -> a
extractExecutionState (Rewritten a) = a
extractExecutionState (Remaining a) = a
extractExecutionState (StartExec a) = a

{- | A strategy primitive: a rewrite rule or builtin simplification step.
 -}
data Prim rewrite = Begin | Simplify | Rewrite !rewrite
    deriving (Show)

-- | Begin the strategy step.
begin :: Prim rewrite
begin = Begin

-- | Apply the rewrite.
rewrite :: rewrite -> Prim rewrite
rewrite = Rewrite

-- | Apply builtin simplification rewrites and evaluate functions.
simplify :: Prim rewrite
simplify = Simplify

executionStrategy :: Stream (Strategy ExecutionPrim)
executionStrategy =
    (Strategy.sequence . fmap Strategy.apply)
        [ BeginExec
        , SimplifyExec
        , ApplyRewrites
        ]
    & Stream.iterate id

{- TODO: docs
-}
data ExecutionPrim
    = BeginExec
    | SimplifyExec
    | ApplyRewrites
    deriving (Show)

{- | A single-step strategy which applies the given rewrite rule.

If the rewrite is successful, the built-in simplification rules and function
evaluator are applied (see 'Pattern.simplify' for details).

 -}
rewriteStep :: rewrite -> Strategy (Prim rewrite)
rewriteStep a =
    Strategy.sequence
        [ Strategy.apply begin
        , Strategy.apply (rewrite a)
        , Strategy.apply simplify
        ]

{- | @TransitionRule@ is the general type of transition rules over 'Prim'.
 -}
type TransitionRule monad rule state =
    Prim rule -> state -> Strategy.TransitionT rule monad state

type ExecutionTransitionRule monad rule state =
    ExecutionPrim -> state -> Strategy.TransitionT rule monad state

transitionRuleSearch
    ::  forall simplifier
    .   MonadSimplify simplifier
    =>  TransitionRule simplifier
            (RewriteRule RewritingVariableName)
            (Pattern RewritingVariableName)
transitionRuleSearch =
    \case
        Simplify -> transitionSimplify
        Rewrite a -> transitionRewrite a
        _ -> pure
  where
    transitionSimplify config = do
        configs <- lift $ Pattern.simplifyTopConfiguration config
        filteredConfigs <- SMT.Evaluator.filterMultiOr configs
        asum (pure <$> toList filteredConfigs)
    transitionRewrite rule config = do
        Transition.addRule rule
        results <-
            Step.applyRewriteRulesParallel
                Unification.unificationProcedure
                [rule]
                config
            & lift
        asum (pure <$> toList (Step.gatherResults results))

{- | Transition rule for primitive strategies in 'Prim'.

@transitionRule@ is intended to be partially applied and passed to
'Strategy.runStrategy'.
 -}
transitionRule
    :: forall simplifier
    .  MonadSimplify simplifier
    => [[RewriteRule RewritingVariableName]]
    -> ExecutionTransitionRule simplifier
            (RewriteRule RewritingVariableName)
            (ExecutionState (Pattern RewritingVariableName))
transitionRule rewriteGroups = transitionRuleWorker
  where
    transitionRuleWorker BeginExec (Rewritten a) = pure $ StartExec a
    transitionRuleWorker BeginExec (Remaining _) = empty
    transitionRuleWorker BeginExec state = pure state

    transitionRuleWorker SimplifyExec (Rewritten patt) =
        Rewritten <$> transitionSimplify patt
    transitionRuleWorker SimplifyExec (Remaining patt) =
        Remaining <$> transitionSimplify patt
    transitionRuleWorker SimplifyExec (StartExec patt) =
        StartExec <$> transitionSimplify patt

    -- transitionRuleWorker (Rewrite rule) (Rewritten patt) =
    --     transitionRewrite rule patt
    transitionRuleWorker ApplyRewrites (Remaining patt) =
        transitionRewrite rewriteGroups patt
    transitionRuleWorker ApplyRewrites (StartExec patt) =
        transitionRewrite rewriteGroups patt
    transitionRuleWorker ApplyRewrites state = pure state

    transitionSimplify config = do
        configs <- lift $ Pattern.simplifyTopConfiguration config
        filteredConfigs <- SMT.Evaluator.filterMultiOr configs
        asum (pure <$> toList filteredConfigs)

    transitionRewrite
        :: [[RewriteRule RewritingVariableName]]
        -> Pattern RewritingVariableName
        -> TransitionT
            (RewriteRule RewritingVariableName)
            simplifier
            (ExecutionState (Pattern RewritingVariableName))
    transitionRewrite xs config =
        foldM transitionRewrite' (Remaining config) xs
      where
        transitionRewrite' applied rewrites
          | Just conf <- retractApplyRemainder applied = do
            Result.Results { results, remainders } <-
                Step.applyRewriteRulesParallel
                    Unification.unificationProcedure
                    rewrites
                    conf
                & lift
            res <- addResults results <|> addRemainders remainders
            -- traceM
            --     $ "\n\nWhen trying to apply rule:" <> unparseToString rule
            --     <> "\nThe result was:\n" <> show (fmap unparseToString res)
            pure res
          | otherwise = pure applied
          where
            addResults results = asum (addResult <$> results)
            addResult Result.Result { appliedRule, result } = do
                addRule (RewriteRule $ extract appliedRule)
                x <- asum (pure . Rewritten <$> toList result)
                -- traceM $ show (fmap unparseToString x) <> "\n\n"
                pure x
            addRemainders remainders = do
                x <- asum (pure . Remaining <$> toList remainders)
                -- traceM $ show (fmap unparseToString x) <> "\n\n"
                pure x

retractApplyRemainder :: ExecutionState a -> Maybe a
retractApplyRemainder (Remaining a) = Just a
retractApplyRemainder _ = Nothing

{- | A strategy that applies all the rewrites in parallel.

After each successful rewrite, the built-in simplification rules and function
evaluator are applied (see 'Pattern.simplify' for details).

See also: 'Strategy.all'

 -}
allRewrites
    :: [rewrite]
    -> Strategy (Prim rewrite)
allRewrites rewrites =
    Strategy.all (rewriteStep <$> rewrites)

{- | A strategy that applies the rewrites until one succeeds.

The rewrites are attempted in order until one succeeds. After a successful
rewrite, the built-in simplification rules and function evaluator are applied
(see 'Pattern.simplify' for details).

See also: 'Strategy.any'

 -}
anyRewrite
    :: [rewrite]
    -> Strategy (Prim rewrite)
anyRewrite rewrites =
    Strategy.any (rewriteStep <$> rewrites)

priorityAllStrategy
    :: From rewrite Attribute.PriorityAttributes
    => [rewrite]
    -> Strategy (Prim rewrite)
priorityAllStrategy rewrites =
    Strategy.first (fmap allRewrites priorityGroups)
  where
    priorityGroups = groupSortOn Attribute.getPriorityOfAxiom rewrites

priorityAnyStrategy
    :: From rewrite Attribute.PriorityAttributes
    => [rewrite]
    -> Strategy (Prim rewrite)
priorityAnyStrategy rewrites =
    anyRewrite sortedRewrites
  where
    sortedRewrites = sortOn Attribute.getPriorityOfAxiom rewrites
