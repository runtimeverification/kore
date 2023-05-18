{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.ModelChecker.Step (
    -- * Primitive strategies
    Prim (..),
    ModalPattern (..),
    CommonModalPattern,
    ProofState (..),
    CommonProofState,
    Transition,
    transitionRule,
    defaultOneStepStrategy,
) where

import Control.Monad.State.Strict (
    StateT,
 )
import Control.Monad.State.Strict qualified as State
import Data.Binary (Binary)
import Data.Text (
    Text,
 )
import GHC.Generics
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.TermLike (
    TermLike,
 )
import Kore.Log.DecidePredicateUnknown (srcLoc)
import Kore.ModelChecker.Simplification (
    checkImplicationIsTop,
 )
import Kore.Rewrite.Result qualified as StepResult
import Kore.Rewrite.RewriteStep qualified as Step
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.RulePattern (
    RewriteRule (RewriteRule),
    allPathGlobally,
 )
import Kore.Rewrite.SMT.Evaluator qualified as SMT.Evaluator (
    filterMultiOr,
 )
import Kore.Rewrite.Strategy (
    Step,
    TransitionT,
 )
import Kore.Simplify.Pattern qualified as Pattern (
    simplifyTopConfiguration,
 )
import Kore.Simplify.Simplify (
    Simplifier,
    liftSimplifier,
 )
import Kore.TopBottom
import Prelude.Kore
import Pretty qualified

data Prim patt rewrite
    = -- | Check the proof state and decide whether to terminate the computation
      CheckProofState
    | -- | Builtin and function symbol simplification step
      Simplify
    | -- | Unroll the proof goal
      Unroll !patt
    | -- | Compute next states
      ComputeWeakNext ![rewrite]
    deriving stock (Show)

data ModalPattern variable = ModalPattern
    { modalOp :: !Text
    , term :: !(TermLike variable)
    }

deriving stock instance Eq variable => Eq (ModalPattern variable)
deriving stock instance Show variable => Show (ModalPattern variable)

type CommonModalPattern = ModalPattern RewritingVariableName

data ProofState patt
    = Proven
    | Unprovable !patt
    | -- | State on which a normal 'Rewrite' can be applied. Also used
      -- for the start patterns.
      GoalLHS !patt
    | -- | State which can't be rewritten anymore.
      GoalRemLHS !patt
    deriving stock (Show, Eq, Ord, Generic, Functor)
    deriving anyclass (Binary)

instance TopBottom (ProofState patt) where
    isTop _ = False
    isBottom _ = False

-- | A 'ProofState' instantiated to 'Pattern RewritingVariableName' for convenience.
type CommonProofState = ProofState (Pattern RewritingVariableName)

instance Hashable patt => Hashable (ProofState patt)

checkProofState :: Prim patt rewrite
checkProofState = CheckProofState

simplify :: Prim patt rewrite
simplify = Simplify

unroll :: patt -> Prim patt rewrite
unroll = Unroll

computeWeakNext :: [rewrite] -> Prim patt rewrite
computeWeakNext = ComputeWeakNext

type Transition m =
    TransitionT (RewriteRule RewritingVariableName) (StateT (Maybe ()) m)

transitionRule ::
    Prim CommonModalPattern (RewriteRule RewritingVariableName) ->
    CommonProofState ->
    Transition Simplifier CommonProofState
transitionRule
    strategyPrim
    proofState =
        case strategyPrim of
            CheckProofState -> transitionCheckProofState proofState
            Simplify -> transitionSimplify proofState
            Unroll goalrhs -> transitionUnroll goalrhs proofState
            ComputeWeakNext rewrites ->
                transitionComputeWeakNext rewrites proofState
      where
        transitionCheckProofState ::
            CommonProofState ->
            Transition Simplifier CommonProofState
        transitionCheckProofState proofState0 = do
            execState <- lift State.get
            -- End early if any unprovable state was reached
            when (isJust execState) empty
            case proofState0 of
                Proven -> empty
                Unprovable _ -> empty
                ps -> return ps

        transitionSimplify ::
            CommonProofState ->
            Transition Simplifier CommonProofState
        transitionSimplify Proven = return Proven
        transitionSimplify (Unprovable config) = return (Unprovable config)
        transitionSimplify (GoalLHS config) =
            applySimplify GoalLHS config
        transitionSimplify (GoalRemLHS config) =
            applySimplify GoalRemLHS config

        applySimplify wrapper config =
            do
                configs <-
                    lift . lift $
                        Pattern.simplifyTopConfiguration config
                filteredConfigs <- liftSimplifier $ SMT.Evaluator.filterMultiOr $srcLoc configs
                if null filteredConfigs
                    then return Proven
                    else
                        asum $
                            pure . wrapper
                                <$> toList filteredConfigs

        transitionUnroll ::
            CommonModalPattern ->
            CommonProofState ->
            Transition Simplifier CommonProofState
        transitionUnroll _ Proven = empty
        transitionUnroll _ (Unprovable _) = empty
        transitionUnroll goalrhs (GoalLHS config)
            | Pattern.isBottom config = return Proven
            | otherwise = applyUnroll goalrhs GoalLHS config
        transitionUnroll goalrhs (GoalRemLHS config)
            | Pattern.isBottom config = return Proven
            | otherwise = applyUnroll goalrhs GoalRemLHS config

        applyUnroll ModalPattern{modalOp, term} wrapper config
            | modalOp == allPathGlobally = do
                result <-
                    lift . lift $
                        checkImplicationIsTop config term
                if result
                    then return (wrapper config)
                    else do
                        (lift . State.put) (Just ())
                        return (Unprovable config)
            | otherwise =
                (error . show . Pretty.vsep)
                    [ "Not implemented error:"
                    , "We don't know how to unroll the modalOp:"
                    , Pretty.pretty modalOp
                    ]

        transitionComputeWeakNext ::
            [RewriteRule RewritingVariableName] ->
            CommonProofState ->
            Transition Simplifier CommonProofState
        transitionComputeWeakNext _ Proven = return Proven
        transitionComputeWeakNext _ (Unprovable config) = return (Unprovable config)
        transitionComputeWeakNext rules (GoalLHS config) =
            transitionComputeWeakNextHelper rules config
        transitionComputeWeakNext _ (GoalRemLHS pat) =
            let patSort = Pattern.patternSort pat
             in return (GoalLHS (Pattern.bottomOf patSort))

        transitionComputeWeakNextHelper ::
            [RewriteRule RewritingVariableName] ->
            Pattern RewritingVariableName ->
            Transition Simplifier CommonProofState
        transitionComputeWeakNextHelper _ config
            | Pattern.isBottom config = return Proven
        transitionComputeWeakNextHelper rules config = do
            results <-
                Step.applyRewriteRulesParallel
                    rules
                    config
                    & lift . lift
            let mapRules =
                    StepResult.mapRules $
                        RewriteRule
                            . Step.withoutUnification
                mapConfigs =
                    StepResult.mapConfigs
                        GoalLHS
                        GoalRemLHS
            StepResult.transitionResults (mapConfigs $ mapRules results)

defaultOneStepStrategy ::
    -- | The modal pattern.
    patt ->
    -- | normal rewrites
    [rewrite] ->
    Step (Prim patt rewrite)
defaultOneStepStrategy goalrhs rewrites =
    [ checkProofState
    , simplify
    , (unroll goalrhs)
    , (computeWeakNext rewrites)
    , simplify
    ]
