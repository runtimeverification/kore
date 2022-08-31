{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Reachability.Claim (
    Claim (..),
    ApplyResult (..),
    AppliedRule (..),
    retractApplyRemainder,
    strategy,
    MinDepth (..),
    strategyWithMinDepth,
    TransitionRule,
    Prim,
    ClaimExtractor (..),
    WithConfiguration (..),
    CheckImplicationResult (..),
    extractClaims,
    reachabilityFirstStep,
    reachabilityNextStep,
    reachabilityCheckOnly,
    transitionRule,
    isTrusted,
    StuckCheck (..),

    -- * Re-exports
    RewriteRule (..),
    module Kore.Log.InfoReachability,

    -- * For Claim implementations
    deriveSeqClaim,
    checkImplication',
    simplify',
    derivePar',
    deriveSeq',

    -- * For JSON-RPC
    checkSimpleImplication,
    ImplicationError,

    -- * For testing
    checkImplicationWorker,
    simplifyRightHandSide,
) where

import Control.Lens (
    Lens',
 )
import Control.Lens qualified as Lens
import Control.Monad.Catch (
    Exception (..),
    SomeException (..),
 )
import Control.Monad.Except (
    ExceptT,
 )
import Control.Monad.State.Strict (
    MonadState,
    StateT,
    runStateT,
 )
import Control.Monad.State.Strict qualified as State
import Data.Functor.Compose
import Data.Generics.Product (
    field,
 )
import Data.List (intersect, partition, (\\))
import Data.Monoid qualified as Monoid
import Data.Stream.Infinite (
    Stream (..),
 )
import Data.Stream.Infinite qualified as Stream
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute.Axiom
import Kore.Attribute.Label qualified as Attribute (
    Label,
 )
import Kore.Attribute.Pattern.FreeVariables (getFreeElementVariables)
import Kore.Attribute.RuleIndex qualified as Attribute (
    RuleIndex,
 )
import Kore.Attribute.SourceLocation qualified as Attribute (
    SourceLocation,
 )
import Kore.Attribute.Trusted qualified as Attribute.Trusted
import Kore.Error (Error (..), koreFailWhen, withContext)
import Kore.IndexedModule.IndexedModule (
    IndexedModule (indexedModuleClaims),
    VerifiedModule,
 )
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrPattern (
    OrPattern,
 )
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    makeCeilPredicate,
    makeInPredicate,
 )
import Kore.Internal.SideCondition (
    SideCondition,
 )
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Internal.TermLike (
    Not (..),
    Sort,
    TermLike,
    freeVariables,
    isFunctionPattern,
    mkExistsN,
    mkImplies,
    termLikeSort,
 )
import Kore.Log.InfoReachability
import Kore.Log.WarnClaimRHSIsBottom
import Kore.Reachability.ClaimState hiding (
    claimState,
 )
import Kore.Reachability.Prim
import Kore.Rewrite.AxiomPattern (
    AxiomPattern (..),
 )
import Kore.Rewrite.ClaimPattern (
    ClaimPattern (..),
 )
import Kore.Rewrite.ClaimPattern qualified as ClaimPattern
import Kore.Rewrite.Result (
    Result (..),
    Results (..),
 )
import Kore.Rewrite.RewriteStep qualified as Step
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.RulePattern (
    RewriteRule (..),
    RulePattern (..),
 )
import Kore.Rewrite.SMT.Evaluator qualified as SMT.Evaluator
import Kore.Rewrite.Step qualified as Step
import Kore.Rewrite.Strategy (
    Strategy,
 )
import Kore.Rewrite.Strategy qualified as Strategy
import Kore.Rewrite.Transition qualified as Transition
import Kore.Simplify.API (
    MonadSimplify,
    liftSimplifier,
 )
import Kore.Simplify.Exists qualified as Exists
import Kore.Simplify.Not qualified as Not
import Kore.Simplify.Pattern (
    simplifyTopConfigurationDefined,
 )
import Kore.Simplify.Pattern qualified as Pattern
import Kore.Syntax.Variable
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unification.Procedure (unificationProcedure)
import Kore.Unification.UnifierT as UnifierT
import Kore.Unparser (
    Unparse (..),
 )
import Kore.Util.TSM.UnifyTag qualified as Tag (CheckImplTag(..))
import Kore.Verified qualified as Verified
import Logic qualified
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

class Claim claim where
    -- | @Rule claim@ is the type of rule to take a single step toward @claim@.
    data Rule claim

    checkImplication ::
        MonadSimplify m =>
        claim ->
        LogicT m (CheckImplicationResult claim)

    simplify ::
        MonadSimplify m =>
        claim ->
        Strategy.TransitionT (AppliedRule claim) m claim

    applyClaims ::
        MonadSimplify m =>
        [claim] ->
        claim ->
        Strategy.TransitionT (AppliedRule claim) m (ApplyResult claim)

    applyAxioms ::
        MonadSimplify m =>
        [[Rule claim]] ->
        claim ->
        Strategy.TransitionT (AppliedRule claim) m (ApplyResult claim)

{- | 'ApplyResult' is the result of a rewriting step, like 'applyClaims' or 'applyAxioms'.

    Both 'ApplyRewritten' and 'ApplyRemainder' wrap a newly formed claim.
    Its left hand side is constructed from either the application of rewrite rules,
    or, respectively, from the remainder resulting after this procedure.
-}
data ApplyResult claim
    = ApplyRewritten !claim
    | ApplyRemainder !claim
    deriving stock (Show, Eq)
    deriving stock (Functor)

-- | 'AppliedRule' represents the rule applied during a rewriting step.
data AppliedRule claim
    = AppliedAxiom (Rule claim)
    | AppliedClaim claim
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance (Debug claim, Debug (Rule claim)) => Debug (AppliedRule claim)

instance
    ( Diff claim
    , Debug claim
    , Diff (Rule claim)
    , Debug (Rule claim)
    ) =>
    Diff (AppliedRule claim)

instance
    (From claim Attribute.Label, From (Rule claim) Attribute.Label) =>
    From (AppliedRule claim) Attribute.Label
    where
    from (AppliedAxiom rule) = from rule
    from (AppliedClaim claim) = from claim

instance
    (From claim Attribute.RuleIndex, From (Rule claim) Attribute.RuleIndex) =>
    From (AppliedRule claim) Attribute.RuleIndex
    where
    from (AppliedAxiom rule) = from rule
    from (AppliedClaim claim) = from claim

instance
    ( From claim Attribute.SourceLocation
    , From (Rule claim) Attribute.SourceLocation
    ) =>
    From (AppliedRule claim) Attribute.SourceLocation
    where
    from (AppliedAxiom rule) = from rule
    from (AppliedClaim claim) = from claim

instance (Unparse claim, Unparse (Rule claim)) => Unparse (AppliedRule claim) where
    unparse (AppliedAxiom rule) = unparse rule
    unparse (AppliedClaim claim) = unparse claim

    unparse2 (AppliedAxiom rule) = unparse2 rule
    unparse2 (AppliedClaim claim) = unparse2 claim

type AxiomAttributes = Attribute.Axiom.Axiom Symbol VariableName

class ClaimExtractor claim where
    extractClaim :: (AxiomAttributes, Verified.SentenceClaim) -> Maybe claim

-- | Extracts all One-Path claims from a verified module.
extractClaims ::
    ClaimExtractor claim =>
    -- | 'IndexedModule' containing the definition
    VerifiedModule declAtts ->
    [claim]
extractClaims = mapMaybe extractClaim . indexedModuleClaims

deriveSeqClaim ::
    MonadSimplify m =>
    Step.UnifyingRule claim =>
    Step.UnifyingRuleVariable claim ~ RewritingVariableName =>
    From claim (AxiomPattern RewritingVariableName) =>
    From claim Attribute.SourceLocation =>
    Lens' claim ClaimPattern ->
    (ClaimPattern -> claim) ->
    [claim] ->
    claim ->
    Strategy.TransitionT (AppliedRule claim) m (ApplyResult claim)
deriveSeqClaim lensClaimPattern mkClaim claims claim =
    getCompose $
        Lens.forOf lensClaimPattern claim $
            \claimPattern ->
                fmap (snd . Step.refreshRule mempty) $
                    Lens.forOf (field @"left") claimPattern $
                        \config -> Compose $ do
                            let claimPatSort = ClaimPattern.getClaimPatternSort claimPattern
                            results <-
                                Step.applyClaimsSequence
                                    mkClaim
                                    config
                                    (Lens.view lensClaimPattern <$> claims)
                                    & lift
                            deriveResults claimPatSort fromAppliedRule results
  where
    fromAppliedRule =
        AppliedClaim
            . mkClaim
            . Step.withoutUnification

type TransitionRule m rule state =
    Prim -> state -> Strategy.TransitionT rule m state

data StuckCheck
    = EnabledStuckCheck
    | DisabledStuckCheck
    deriving stock (Eq)

transitionRule ::
    forall m claim.
    MonadSimplify m =>
    Claim claim =>
    StuckCheck ->
    [claim] ->
    [[Rule claim]] ->
    TransitionRule m (AppliedRule claim) (ClaimState claim)
transitionRule stuckCheck claims axiomGroups = transitionRuleWorker
  where
    transitionRuleWorker ::
        Prim ->
        ClaimState claim ->
        Strategy.TransitionT (AppliedRule claim) m (ClaimState claim)

    transitionRuleWorker Begin Proven = empty
    transitionRuleWorker Begin (Stuck _) = empty
    transitionRuleWorker Begin (Rewritten claim) = pure (Claimed claim)
    transitionRuleWorker Begin claimState = pure claimState
    transitionRuleWorker Simplify claimState
        | Just claim <- retractSimplifiable claimState =
            Transition.ifte (simplify claim) (pure . ($>) claimState) (pure Proven)
        | otherwise =
            pure claimState
    transitionRuleWorker CheckImplication claimState
        | Just claim <- retractRewritable claimState = do
            result <- checkImplication claim & Logic.lowerLogicT
            case result of
                Implied _ -> pure Proven
                NotImpliedStuck a ->
                    returnStuckOrContinue a
                NotImplied a
                    | isRemainder claimState -> do
                        pure (Stuck a)
                    | otherwise -> pure (Claimed a)
        | otherwise = pure claimState
    transitionRuleWorker ApplyClaims (Claimed claim) =
        Transition.ifte
            (applyClaims claims claim)
            (return . applyResultToClaimState)
            (pure Proven)
    transitionRuleWorker ApplyClaims claimState = pure claimState
    transitionRuleWorker ApplyAxioms claimState
        | Just claim <- retractRewritable claimState =
            Transition.ifte
                (applyAxioms axiomGroups claim)
                (return . applyResultToClaimState)
                (pure Proven)
        | otherwise = pure claimState

    applyResultToClaimState (ApplyRewritten a) = Rewritten a
    applyResultToClaimState (ApplyRemainder a) = Remaining a

    returnStuckOrContinue state =
        case stuckCheck of
            EnabledStuckCheck -> return (Stuck state)
            DisabledStuckCheck -> return (Claimed state)

retractSimplifiable :: ClaimState a -> Maybe a
retractSimplifiable (Claimed a) = Just a
retractSimplifiable (Rewritten a) = Just a
retractSimplifiable (Remaining a) = Just a
retractSimplifiable _ = Nothing

retractApplyRemainder :: ApplyResult a -> Maybe a
retractApplyRemainder (ApplyRemainder a) = Just a
retractApplyRemainder _ = Nothing

isRemainder :: ClaimState a -> Bool
isRemainder (Remaining _) = True
isRemainder _ = False

reachabilityFirstStep :: Strategy Prim
reachabilityFirstStep =
    (Strategy.sequence . map Strategy.apply)
        [ Begin
        , Simplify
        , CheckImplication
        , ApplyAxioms
        , Simplify
        ]

reachabilityNextStep :: Strategy Prim
reachabilityNextStep =
    (Strategy.sequence . map Strategy.apply)
        [ Begin
        , Simplify
        , CheckImplication
        , ApplyClaims
        , ApplyAxioms
        , Simplify
        ]

reachabilityFirstStepNoCheck :: Strategy Prim
reachabilityFirstStepNoCheck =
    (Strategy.sequence . map Strategy.apply)
        [ Begin
        , Simplify
        , ApplyAxioms
        , Simplify
        ]

reachabilityNextStepNoCheck :: Strategy Prim
reachabilityNextStepNoCheck =
    (Strategy.sequence . map Strategy.apply)
        [ Begin
        , Simplify
        , ApplyClaims
        , ApplyAxioms
        , Simplify
        ]

{- | A strategy for the last step of depth-limited reachability proofs.
   The final such step should only perform a CheckImplication.
-}
reachabilityCheckOnly :: Strategy Prim
reachabilityCheckOnly =
    Strategy.sequence [Strategy.apply Begin, Strategy.apply CheckImplication]

strategy :: Stream (Strategy Prim)
strategy =
    reachabilityFirstStep :> Stream.iterate id reachabilityNextStep

newtype MinDepth = MinDepth
    { getMinDepth :: Int
    }

strategyWithMinDepth :: MinDepth -> Stream (Strategy Prim)
strategyWithMinDepth (MinDepth minDepth) =
    Stream.prepend
        noCheckReachabilitySteps
        reachabilitySteps
  where
    noCheckReachabilitySteps =
        reachabilityFirstStepNoCheck :
        replicate (minDepth - 1) reachabilityNextStepNoCheck
    reachabilitySteps =
        Stream.iterate id reachabilityNextStep

{- | The result of checking the direct implication of a proof claim.

As an optimization, 'checkImplication' returns 'NotImpliedStuck' when the
implication between /terms/ is valid, but the implication between side
conditions does not hold.
-}
data CheckImplicationResult a
    = -- | The implication is valid.
      Implied !a
    | -- | The implication is not valid.
      NotImplied !a
    | -- | The implication between /terms/ is valid, but the implication between
      -- side-conditions is not valid.
      NotImpliedStuck !a
    deriving stock (Eq, Ord, Show)
    deriving stock (Foldable, Functor, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Pretty a => Pretty (CheckImplicationResult a) where
    pretty (Implied a) =
        Pretty.vsep
            [ "implied:"
            , Pretty.indent 4 $ pretty a
            ]
    pretty (NotImplied a) =
        Pretty.vsep
            [ "not implied:"
            , Pretty.indent 4 $ pretty a
            ]
    pretty (NotImpliedStuck a) =
        Pretty.vsep
            [ "stuck:"
            , Pretty.indent 4 $ pretty a
            ]

-- | Remove the destination of the claim.
checkImplication' ::
    forall claim m.
    (MonadLogic m, MonadSimplify m) =>
    Lens' claim ClaimPattern ->
    claim ->
    m (CheckImplicationResult claim)
checkImplication' lensRulePattern claim =
    claim
        & Lens.traverseOf lensRulePattern (Compose . checkImplicationWorker)
        & getCompose

assertFunctionLikeConfiguration ::
    forall m.
    Monad m =>
    HasCallStack =>
    ClaimPattern ->
    m ()
assertFunctionLikeConfiguration claimPattern
    | (not . isFunctionPattern) leftTerm =
        error . show . Pretty.vsep $
            [ "The check implication step expects\
              \ the configuration term to be function-like."
            , Pretty.indent 2 "Configuration term:"
            , Pretty.indent 4 (unparse leftTerm)
            ]
    | otherwise = pure ()
  where
    ClaimPattern{left} = claimPattern
    leftTerm = Pattern.term left

newtype AnyUnified = AnyUnified {didAnyUnify :: Bool}
    deriving stock (Eq, Ord, Read, Show)
    deriving (Semigroup, Monoid) via Monoid.Any

{- | Check the claim by direct implication.

The claim has the form

@
φ(X) → ∘ ∃ Y. ⋁ᵢ ψᵢ(X, Y)
@

where @∘ _@ is a modality in reachability logic. @φ@ and the @ψᵢ@ are assumed to
be function-like patterns. @X@ and @Y@ are disjoint families of
variables. @checkImplicationWorker@ checks the validity of the formula

@
⌊ φ(X) → ∃ Y. ⋁ᵢ ψᵢ(X, Y) ⌋
@

Let @φ(X) = t(X) ∧ P(X)@ and @ψᵢ(X, Y) = tᵢ(X, Y) ∧ Pᵢ(X, Y)@; then the
implication formula above is valid when

@
(⋀ᵢ ¬ ∃ Y. ⌈t(X) ∧ tᵢ(X, Y)⌉ ∧ Pᵢ(X, Y)) ∧ ⌈t(X)⌉ ∧ P(X)
@

is unsatisfiable. This predicate basically consists of two parts: a single positive
conjunct asserting that the left-hand side of the claim is satisfiable:

@
⌈t(X)⌉ ∧ P(X)
@

and many negative conjuncts arising from the unification of the left- and
right-hand sides:

@
⋀ᵢ ¬ ∃ Y. ⌈t(X) ∧ tᵢ(X, Y)⌉ ∧ Pᵢ(X, Y)
@

When the implication formula is valid, @checkImplicationWorker@ returns
'Implied'. When the implication formula is not valid, we apply the following
heuristic:

* If any of the unification problems @⌈t(X) ∧ tᵢ(X, Y)⌉@ succeeded,
  @checkImplicationWorker@ returns 'NotImpliedStuck',
* otherwise, it returns 'NotImplied'.

Returing 'NotImpliedStuck' has the effect of terminating the proof. This
heuristic prevents the prover from executing beyond the intended final program
state ("inventing" programs), but at the cost that it does prevent the prover
from visiting the final program state twice. In practice, we find that deductive
proofs should not require the prover to visit the final program state twice,
anyway.
-}
checkImplicationWorker ::
    forall m.
    (MonadIO m, MonadLogic m, MonadSimplify m) =>
    ClaimPattern ->
    m (CheckImplicationResult ClaimPattern)
checkImplicationWorker (ClaimPattern.refreshExistentials -> claimPattern) = do
      marker Tag.StartCheck
      definedConfig <-
            Pattern.simplify $
                Pattern.fromCondition sort $
                    leftCondition <> from (makeCeilPredicate leftTerm)
      isTrivial <-
            fmap isBottom $ (liftSimplifier . SMT.Evaluator.filterMultiOr) definedConfig
      if isTrivial
      then do
       marker Tag.Implied
       pure (Implied claimPattern)
      else do
       marker Tag.AttemptUnify
       (anyUnified, removal) <- getNegativeConjuncts
       -- since we have already checked definedConfig, abort if nothing was unified
       if (not $ didAnyUnify anyUnified)
       then do
        marker Tag.NotImplied
        if (isBottom right)
        then warnClaimRHSIsBottom claimPattern >> pure (NotImpliedStuck claimPattern)
        else pure (NotImplied claimPattern)
       else do
        marker Tag.BuiltToRefute
        let configs' = MultiOr.map (OrPattern.toPattern sort definedConfig <*) removal
        stuck <-
            Logic.scatter configs'
                >>= Pattern.simplify
                >>= (marker Tag.ReadyToRefute *>) .
                    liftSimplifier . SMT.Evaluator.filterMultiOr
                >>= Logic.scatter
        marker Tag.CouldNotRefute
        examine anyUnified stuck
        & elseImplied
  where
    marker tag = liftIO . traceMarkerIO $ concat ["check-implication:", show tag, ":"]

    ClaimPattern{right, left, existentials} = claimPattern
    leftTerm = Pattern.term left
    sort = termLikeSort leftTerm
    leftCondition = Pattern.withoutTerm left

    -- TODO (#1278): Do not combine the predicate and the substitution.
    -- This is held over from the old representation of claims, which did not
    -- distinguish the predicate and substitution in the first place. We can't
    -- use the substitution directly yet, because it isn't kept normalized. Once
    -- the claim is fully simplified at every step, that should not be a
    -- problem.
    sideCondition =
        SideCondition.fromConditionWithReplacements
            ( Condition.fromPredicate
                . Condition.toPredicate
                $ leftCondition
            )

    getNegativeConjuncts :: m (AnyUnified, OrPattern RewritingVariableName)
    getNegativeConjuncts =
        do
            assertFunctionLikeConfiguration claimPattern
            right' <- Logic.scatter right
            let (rightTerm, rightCondition) = Pattern.splitTerm right'
            unified <-
                makeInPredicate leftTerm rightTerm
                    & Pattern.fromPredicateSorted sort
                    & Pattern.simplify
                    & (>>= Logic.scatter)
            didUnify
            marker Tag.CouldUnify
            removed <-
                Pattern.andCondition unified rightCondition
                    & Pattern.simplify
                    & (>>= Logic.scatter)
            Exists.makeEvaluate sideCondition existentials removed
                >>= Logic.scatter
            & OrPattern.observeAllT
            & (>>= mkNotSimplified)
            & wereAnyUnified
      where
        mkNotSimplified notChild =
            Not.simplify sideCondition Not{notSort = sort, notChild}

    wereAnyUnified :: StateT AnyUnified m a -> m (AnyUnified, a)
    wereAnyUnified act = swap <$> runStateT act mempty

    didUnify :: MonadState AnyUnified state => state ()
    didUnify = State.put (AnyUnified True)

    elseImplied acts =
        Logic.ifte
            acts
            (\result -> marker Tag.NotImplied *> pure result)
            (marker Tag.Implied >> pure (Implied claimPattern))

    examine ::
        AnyUnified ->
        Pattern RewritingVariableName ->
        m (CheckImplicationResult ClaimPattern)
    examine AnyUnified{didAnyUnify} stuck
        | didAnyUnify
          , isBottom condition =
            pure $ Implied claimPattern
        | not didAnyUnify
          , not (isBottom right) =
            pure $ NotImplied claimPattern
        | otherwise = do
            when (isBottom right) $
                warnClaimRHSIsBottom claimPattern
            pure $
                Lens.set (field @"left") stuck claimPattern
                    & NotImpliedStuck
      where
        (_, condition) = Pattern.splitTerm stuck

{- | Check a simple implication (with a single consequent), and return a
  substitution that witnesses it if the implication is true.

Requirements:
* The LHS pattern must be function-like
* Free variables of the LHS must not be used as existentials on the RHS
* Both LHS and RHS must simplify to single patterns (OrPattern singletons)

The implication has the form

@
φ(X) → ∃ Y. ψ(X, Y)
@

again with @X@ and @Y@ disjoint families of variables, and @φ@ and @ψ@
assumed to be function-like patterns,

Let @φ(X) = t(X) ∧ P(X)@ and @ψ(X, Y) = t'(X, Y) ∧ P'(X, Y)@. Following
the reasoning from above in @'checkImplicationWorker'@, the
implication is valid when

@
(¬ ∃ Y. ⌈t(X) ∧ t'(X, Y)⌉ ∧ P'(X, Y)) ∧ ⌈t(X)⌉ ∧ P(X)
@

is unsatisfiable.

1) If

@
⌈t(X)⌉ ∧ P(X)
@

is already unsatisfiable by itself, the implication holds trivially
(ex falso quodlibet) with an empty substitution.

2) Otherwise, we must refute

@
¬ ∃ Y. ⌈t(X) ∧ t'(X, Y)⌉ ∧ P'(X, Y) ∧ ⌈t(X)⌉ ∧ P(X)
@

which involves unification ⌈t(X) ∧ t'(X, Y)⌉ providing the desired
substitution.

When the implication formula is valid, @checkImplicationWorker@
returns 'Implied' with a witness substitution.

When the implication formula is not valid, the function behaves
similar to @checkImplicationWorker@:

* If unification @⌈t(X) ∧ t'(X, Y)⌉@ succeeded, the return value is
  'NotImpliedStuck' with terms that could not be refuted, and the
  substitution for the unification.
* otherwise, the function returns 'NotImplied' without a substitution.
-}
checkSimpleImplication ::
    forall m v.
    (MonadSimplify m) =>
    (v ~ RewritingVariableName) =>
    Pattern v -> -- left
    Pattern v -> -- right
    [ElementVariable v] -> -- existentials
    ExceptT
        (Error ImplicationError)
        m
        (TermLike v, CheckImplicationResult (Maybe (Pattern.Condition v)))
checkSimpleImplication inLeft inRight existentials =
    do
        left <- simplifyToSingle "LHS: " inLeft
        right <- simplifyToSingle "RHS: " inRight
        checkAssumptions left right
        let (rightTerm, rightCondition) = Pattern.splitTerm right
            (leftTerm, leftCondition) = Pattern.splitTerm left
            claimToCheck =
                -- this will be returned with the result
                mkImplies
                    (Pattern.toTermLike left)
                    (mkExistsN existentials (Pattern.toTermLike right))

        let definedConfig =
                Pattern.andCondition left $
                    from $ makeCeilPredicate leftTerm
        trivial <-
            fmap isBottom $
                (liftSimplifier . SMT.Evaluator.filterMultiOr)
                    =<< Pattern.simplify definedConfig

        if trivial
            then pure (claimToCheck, Implied . Just $ Condition.top) -- trivial unifier
            else do
                -- attempt term unification (to remember the substitution
                unified <-
                    Logic.observeAllT $
                        evalEnvUnifierT
                            Not.notSimplifier
                            (unificationProcedure SideCondition.top leftTerm rightTerm)

                -- for each unification result, attempt to refute the formula
                remainders ::
                    [(OrPattern v, Pattern.Condition v)] <-
                    mapM (lift . checkUnifiedConsequent definedConfig leftCondition rightCondition) unified

                let (successes, stucks) = partition (isBottom . fst) remainders

                pure . (claimToCheck,) $ case (successes, stucks) of
                    ((_bottom, unifier) : _, _) ->
                        -- success for at least one, return (ignore other failures)
                        Implied $ Just unifier
                    ([], []) ->
                        -- no successful unification, thus not implied
                        NotImplied Nothing
                    ([], (_stuck, unifier) : _) ->
                        -- successful unification but all stuck,
                        -- return original left term and unifier
                        NotImpliedStuck $ Just unifier
  where
    sort = termLikeSort (Pattern.term inLeft)

    simplifyToSingle ::
        String ->
        Pattern v ->
        ExceptT (Error ImplicationError) m (Pattern v)
    simplifyToSingle ctx pat = do
        let patSort = termLikeSort (Pattern.term pat)
        simplified <- toList <$> lift (Pattern.simplify pat)
        withContext (ctx <> showPretty pat) $
            koreFailWhen
                (length simplified > 1)
                "Term does not simplify to a singleton pattern"
        pure $
            fromMaybe (Pattern.bottomOf patSort) $ headMay simplified

    showPretty :: Pretty a => a -> String
    showPretty = Pretty.renderString . Pretty.layoutOneLine . Pretty.pretty

    checkAssumptions left right = do
        let leftTerm = Pattern.term left
        -- must be function-like
        withContext (showPretty leftTerm) $
            koreFailWhen
                (not $ isFunctionPattern leftTerm)
                "The check implication step expects the antecedent term to be function-like."
        -- RHS existentials must not capture free variables of LHS
        let lhsFreeElemVars = getFreeElementVariables (freeVariables left)
            nameCollisions = existentials `intersect` lhsFreeElemVars
            rightTerm = Pattern.term right
        withContext ("LHS: " <> showPretty leftTerm)
            . withContext ("RHS: " <> showPretty rightTerm)
            . withContext ("existentials: " <> show (map unparse2 existentials))
            $ koreFailWhen (not $ null nameCollisions) $
                unwords
                    ( "Existentials capture free variables of the antecedent:" :
                      map (show . unparse2) nameCollisions
                    )
        -- RHS must not have free variables that aren't free in the LHS
        let rhsFreeElemVars = getFreeElementVariables $ freeVariables right
            offending = rhsFreeElemVars \\ (lhsFreeElemVars <> existentials)
        withContext ("LHS: " <> showPretty leftTerm)
            . withContext ("RHS: " <> showPretty rightTerm)
            . withContext ("existentials: " <> show (map unparse2 existentials))
            $ koreFailWhen (not $ null offending) $
                unwords
                    ( "The RHS must not have free variables not present in the LHS:" :
                      map (show . unparse2) offending
                    )
        -- sorts of LHS and RHS have to agree
        let lSort = termLikeSort leftTerm
            rSort = termLikeSort rightTerm
        withContext ("LHS sort: " <> show (unparse2 lSort))
            . withContext ("RHS sort: " <> show (unparse2 rSort))
            $ koreFailWhen
                (lSort /= rSort)
                "Antecedent and consequent must have the same sort."

    -- using a successful unification result (Condition), build term
    -- to refute and try to refute (returning non-refutable parts and
    -- the substitution for unification)
    checkUnifiedConsequent ::
        Pattern v ->
        Pattern.Condition v ->
        Pattern.Condition v ->
        Pattern.Condition v ->
        m (OrPattern v, Pattern.Condition v)
    checkUnifiedConsequent
        definedConfig
        leftCondition
        rightCondition
        unified =
            fmap ((,unified) . MultiOr.mergeAll) $
                Logic.observeAllT $ do
                    let existsChild =
                            Pattern.andCondition
                                (Pattern.fromCondition sort unified)
                                rightCondition
                        sideCondition =
                            SideCondition.fromConditionWithReplacements
                                -- (NB this from . to applies the substitution)
                                (Condition.fromPredicate . Condition.toPredicate $ leftCondition)

                    notChild <- -- ∃ Y. ⌈t(X) ∧ t'(X, Y)⌉ ∧ P'(X, Y) (Or pattern)
                        Exists.makeEvaluate sideCondition existentials existsChild

                    notRhs <- -- not notChild (Or pattern)
                        Not.simplify sideCondition Not{notSort = sort, notChild}

                    let combineWithAntecedent :: Pattern v -> Pattern v
                        combineWithAntecedent = (definedConfig <*)
                    -- (<*) takes the 1st arg.s term but combines
                    -- both arg.s predicate and substitution

                    -- The result's substitution needs to be renormalized,
                    -- which we achieve by simplifying it

                    toRefute <-
                        Pattern.simplify
                            . OrPattern.toPattern sort
                            . MultiOr.map combineWithAntecedent
                            $ notRhs

                    liftSimplifier $ SMT.Evaluator.filterMultiOr toRefute

-- | type tag for errors thrown from the above
data ImplicationError

simplify' ::
    MonadSimplify m =>
    Lens' claim ClaimPattern ->
    claim ->
    Strategy.TransitionT (AppliedRule claim) m claim
simplify' lensClaimPattern claim = do
    claim' <- simplifyLeftHandSide claim
    let claim'' = Lens.over lensClaimPattern applySubstOnRightHandSide claim'
        sideCondition = extractSideCondition claim''
    simplifyRightHandSide lensClaimPattern sideCondition claim''
  where
    applySubstOnRightHandSide :: ClaimPattern -> ClaimPattern
    applySubstOnRightHandSide claimPat@ClaimPattern{left, right} =
        let substitution = Pattern.substitution left
            mapSubstitution ::
                Pattern RewritingVariableName -> Pattern RewritingVariableName
            mapSubstitution = Lens.over (field @"substitution") (substitution <>)
            right' = MultiOr.map mapSubstitution right
         in claimPat{right = right'}
    extractSideCondition =
        SideCondition.fromConditionWithReplacements
            . Pattern.withoutTerm
            . Lens.view (lensClaimPattern . field @"left")

    simplifyLeftHandSide =
        Lens.traverseOf (lensClaimPattern . field @"left") $ \config -> do
            configs <-
                simplifyTopConfigurationDefined
                    config
                    >>= liftSimplifier . SMT.Evaluator.filterMultiOr
                    & lift
            asum (pure <$> toList configs)

simplifyRightHandSide ::
    MonadSimplify m =>
    Lens' claim ClaimPattern ->
    SideCondition RewritingVariableName ->
    claim ->
    m claim
simplifyRightHandSide lensClaimPattern sideCondition =
    Lens.traverseOf (lensClaimPattern . field @"right") $ \dest ->
        OrPattern.observeAllT $
            Logic.scatter dest
                >>= Pattern.makeEvaluate sideCondition . Pattern.requireDefined
                >>= liftSimplifier . SMT.Evaluator.filterMultiOr
                >>= Logic.scatter

isTrusted :: From claim Attribute.Axiom.Trusted => claim -> Bool
isTrusted = Attribute.Trusted.isTrusted . from @_ @Attribute.Axiom.Trusted

-- | Exception that contains the last configuration before the error.
data WithConfiguration
    = WithConfiguration (Pattern VariableName) SomeException
    deriving stock (Show, Typeable)

instance Exception WithConfiguration

-- | Apply 'Rule's to the claim in parallel.
derivePar' ::
    forall m claim.
    MonadSimplify m =>
    Lens' claim ClaimPattern ->
    (RewriteRule RewritingVariableName -> Rule claim) ->
    [RewriteRule RewritingVariableName] ->
    claim ->
    Strategy.TransitionT (AppliedRule claim) m (ApplyResult claim)
derivePar' lensRulePattern mkRule =
    deriveWith lensRulePattern mkRule Step.applyRewriteRulesParallel

type Deriver monad =
    [RewriteRule RewritingVariableName] ->
    Pattern RewritingVariableName ->
    monad (Step.Results (RulePattern RewritingVariableName))

-- | Apply 'Rule's to the claim in parallel.
deriveWith ::
    forall m claim.
    Monad m =>
    Lens' claim ClaimPattern ->
    (RewriteRule RewritingVariableName -> Rule claim) ->
    Deriver m ->
    [RewriteRule RewritingVariableName] ->
    claim ->
    Strategy.TransitionT (AppliedRule claim) m (ApplyResult claim)
deriveWith lensClaimPattern mkRule takeStep rewrites claim =
    getCompose $
        Lens.forOf lensClaimPattern claim $
            \claimPattern ->
                fmap (snd . Step.refreshRule mempty) $
                    Lens.forOf (field @"left") claimPattern $
                        \config -> Compose $ do
                            let claimPatSort = ClaimPattern.getClaimPatternSort claimPattern
                            results <- takeStep rewrites config & lift
                            deriveResults claimPatSort fromAppliedRule results
  where
    fromAppliedRule =
        AppliedAxiom
            . mkRule
            . RewriteRule
            . Step.withoutUnification

-- | Apply 'Rule's to the claim in sequence.
deriveSeq' ::
    forall m claim.
    MonadSimplify m =>
    Lens' claim ClaimPattern ->
    (RewriteRule RewritingVariableName -> Rule claim) ->
    [RewriteRule RewritingVariableName] ->
    claim ->
    Strategy.TransitionT (AppliedRule claim) m (ApplyResult claim)
deriveSeq' lensRulePattern mkRule =
    deriveWith lensRulePattern mkRule $ flip Step.applyRewriteRulesSequence

deriveResults ::
    Step.UnifyingRuleVariable representation ~ RewritingVariableName =>
    Sort ->
    (Step.UnifiedRule representation -> AppliedRule claim) ->
    Step.Results representation ->
    Strategy.TransitionT
        (AppliedRule claim)
        simplifier
        (ApplyResult (Pattern RewritingVariableName))
-- TODO (thomas.tuegel): Remove claim argument.
deriveResults sort fromAppliedRule Results{results, remainders} =
    addResults <|> addRemainders
  where
    addResults = asum (addResult <$> results)
    addRemainders = asum (addRemainder <$> toList remainders)

    addResult Result{appliedRule, result} = do
        addRule appliedRule
        case toList result of
            [] -> addRewritten (Pattern.bottomOf sort)
            configs -> asum (addRewritten <$> configs)

    addRewritten = pure . ApplyRewritten
    addRemainder = pure . ApplyRemainder

    addRule = Transition.addRule . fromAppliedRule
