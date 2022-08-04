{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com

This should be imported qualified.
-}
module Kore.Reachability.Prove (
    CommonClaimState,
    ProveClaimsResult (..),
    StuckClaim (..),
    StuckClaims,
    AllClaims (..),
    Axioms (..),
    ToProve (..),
    AlreadyProven (..),
    proveClaims,
    proveClaimStep,
    lhsClaimStateTransformer,
) where

import Control.DeepSeq (
    deepseq,
 )
import Control.Lens qualified as Lens
import Control.Monad.Catch (
    MonadCatch,
    MonadMask,
    handleAll,
    throwM,
 )
import Control.Monad.Except (
    ExceptT,
    runExceptT,
 )
import Control.Monad.Except qualified as Monad.Except
import Control.Monad.State.Strict (
    StateT,
    runStateT,
 )
import Control.Monad.State.Strict qualified as State
import Data.Graph.Inductive.Graph qualified as Graph
import Data.Limit (
    Limit,
 )
import Data.Limit qualified as Limit
import Data.List.Extra (
    groupSortOn,
    snoc,
 )
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute.Axiom
import Kore.Debug
import Kore.Exec.GraphTraversal qualified as GraphTraversal
import Kore.Internal.Conditional (
    Conditional (..),
 )
import Kore.Internal.MultiAnd (
    MultiAnd,
 )
import Kore.Internal.MultiAnd qualified as MultiAnd
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    Predicate,
    getMultiAndPredicate,
    pattern PredicateCeil,
    pattern PredicateNot,
 )
import Kore.Internal.TermLike (Sort)
import Kore.Log.DebugBeginClaim
import Kore.Log.DebugProven
import Kore.Log.DebugTransition (
    debugAfterTransition,
    debugBeforeTransition,
    debugFinalTransition,
 )
import Kore.Log.InfoProofDepth
import Kore.Log.WarnStuckClaimState
import Kore.Log.WarnTrivialClaim
import Kore.Reachability.Claim
import Kore.Reachability.ClaimState (
    ClaimState,
    ClaimStateTransformer (..),
    extractStuck,
    extractUnproven,
 )
import Kore.Reachability.ClaimState qualified as ClaimState
import Kore.Reachability.Prim as Prim (
    Prim (..),
 )
import Kore.Reachability.SomeClaim
import Kore.Rewrite.ClaimPattern (
    mkGoal,
 )
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName,
    getRewritingPattern,
 )
import Kore.Rewrite.Strategy (
    ExecutionGraph (..),
    FinalNodeType,
    GraphSearchOrder,
    Strategy,
    executionHistoryStep,
 )
import Kore.Rewrite.Transition (
    runTransitionT,
 )
import Kore.Rewrite.Transition qualified as Transition
import Kore.Simplify.API
import Kore.TopBottom
import Kore.Unparser
import Log (
    MonadLog (..),
 )
import Numeric.Natural (
    Natural,
 )
import Prelude.Kore
import Pretty qualified
import Prof

type CommonClaimState = ClaimState.ClaimState SomeClaim

type CommonTransitionRule m =
    TransitionRule m (AppliedRule SomeClaim) CommonClaimState

{- | Extracts the left hand side (configuration) from the
 'CommonClaimState'. If the 'ClaimState' is 'Proven', then
 the configuration will be '\\bottom'.
-}
lhsClaimStateTransformer ::
    Sort ->
    ClaimStateTransformer
        SomeClaim
        (Pattern RewritingVariableName)
lhsClaimStateTransformer sort =
    ClaimStateTransformer
        { claimedTransformer = getConfiguration
        , remainingTransformer = getConfiguration
        , rewrittenTransformer = getConfiguration
        , stuckTransformer = getConfiguration
        , provenValue = Pattern.bottomOf sort
        }

{- | @Verifer a@ is a 'Simplifier'-based action which returns an @a@.

The action may throw an exception if the proof fails; the exception is
a @'MultiAnd'@ of unprovable configuration and a count of unexplored
branches.
-}
type VerifierT m = ExceptT (StuckClaims, Natural) m

newtype AllClaims claim = AllClaims {getAllClaims :: [claim]}
newtype Axioms claim = Axioms {getAxioms :: [Rule claim]}
newtype ToProve claim = ToProve {getToProve :: [(claim, Limit Natural)]}
newtype AlreadyProven = AlreadyProven {getAlreadyProven :: [Text]}

newtype StuckClaim = StuckClaim {getStuckClaim :: SomeClaim}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (NFData, Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance TopBottom StuckClaim where
    isTop = const False
    isBottom = const False

type StuckClaims = MultiAnd StuckClaim

type ProvenClaims = MultiAnd SomeClaim

-- | The result of proving some claims.
data ProveClaimsResult = ProveClaimsResult
    { -- | The conjuction of stuck claims, that is: of claims which must still be
      -- proven. If all claims were proved, then the remaining claims are @\\top@.
      stuckClaims :: !StuckClaims
    , -- | The conjunction of all claims which were proven.
      provenClaims :: !ProvenClaims
    , -- | A count of non-final states that were not explored further
      unexplored :: Natural
    }

proveClaims ::
    Maybe MinDepth ->
    StuckCheck ->
    Limit Natural ->
    GraphSearchOrder ->
    Natural ->
    FinalNodeType ->
    AllClaims SomeClaim ->
    Axioms SomeClaim ->
    AlreadyProven ->
    -- | List of claims, together with a maximum number of verification steps
    -- for each.
    ToProve SomeClaim ->
    Simplifier ProveClaimsResult
proveClaims
    maybeMinDepth
    stuckCheck
    breadthLimit
    searchOrder
    maxCounterexamples
    finalNodeType
    claims
    axioms
    (AlreadyProven alreadyProven)
    (ToProve toProve) =
        do
            (result, provenClaims) <-
                proveClaimsWorker
                    maybeMinDepth
                    stuckCheck
                    breadthLimit
                    searchOrder
                    maxCounterexamples
                    finalNodeType
                    claims
                    axioms
                    unproven
                    & runExceptT
                    & flip runStateT (MultiAnd.make stillProven)
            let (stuckClaims, unexplored) = fromLeft (MultiAnd.top, 0) result
            pure
                ProveClaimsResult
                    { stuckClaims
                    , provenClaims
                    , unexplored
                    }
      where
        unproven :: ToProve SomeClaim
        stillProven :: [SomeClaim]
        (unproven, stillProven) =
            (ToProve newToProve, newAlreadyProven)
          where
            (newToProve, newAlreadyProven) =
                partitionEithers (map lookupEither toProve)
            lookupEither ::
                (SomeClaim, Limit Natural) ->
                Either (SomeClaim, Limit Natural) SomeClaim
            lookupEither claim@(rule, _) =
                if unparseToText2 rule `elem` alreadyProven
                    then Right rule
                    else Left claim

proveClaimsWorker ::
    Maybe MinDepth ->
    StuckCheck ->
    Limit Natural ->
    GraphSearchOrder ->
    Natural ->
    FinalNodeType ->
    AllClaims SomeClaim ->
    Axioms SomeClaim ->
    -- | List of claims, together with a maximum number of verification steps
    -- for each.
    ToProve SomeClaim ->
    VerifierT (StateT ProvenClaims Simplifier) ()
proveClaimsWorker
    maybeMinDepth
    stuckCheck
    breadthLimit
    searchOrder
    maxCounterexamples
    finalNodeType
    claims
    axioms
    (ToProve toProve) =
        traverse_ verifyWorker toProve
      where
        verifyWorker ::
            (SomeClaim, Limit Natural) ->
            VerifierT (StateT ProvenClaims Simplifier) ()
        verifyWorker unprovenClaim@(claim, _) =
            traceExceptT D_OnePath_verifyClaim [debugArg "rule" claim] $ do
                debugBeginClaim claim
                result <-
                    lift . lift $
                        proveClaim
                            maybeMinDepth
                            stuckCheck
                            breadthLimit
                            searchOrder
                            maxCounterexamples
                            finalNodeType
                            claims
                            axioms
                            unprovenClaim
                either
                    -- throw stuck claims, ending the traversal
                    Monad.Except.throwError
                    (const $ addProvenClaim claim)
                    result

        addProvenClaim claim =
            State.modify' (mappend (MultiAnd.singleton claim))

proveClaim ::
    Maybe MinDepth ->
    StuckCheck ->
    Limit Natural ->
    GraphSearchOrder ->
    Natural ->
    FinalNodeType ->
    AllClaims SomeClaim ->
    Axioms SomeClaim ->
    (SomeClaim, Limit Natural) ->
    Simplifier (Either (StuckClaims, Natural) ())
proveClaim
    maybeMinDepth
    stuckCheck
    breadthLimit
    searchOrder
    maxCounterexamples
    finalNodeType
    (AllClaims claims)
    (Axioms axioms)
    (goal, depthLimit) = do
        let startGoal = ClaimState.Claimed (Lens.over lensClaimPattern mkGoal goal)

            limitedStrategyList =
                Limit.takeWithin
                    depthLimit
                    (maybe infinite withMinDepth maybeMinDepth)
                    `snoc` [Begin, CheckImplication] -- last step of limited ste
        traversalResult <-
            GraphTraversal.graphTraversal
                finalNodeType
                searchOrder
                breadthLimit
                transition
                (Limit.Limit maxCounterexamples)
                limitedStrategyList
                (ProofDepth 0, startGoal)

        let returnUnprovenClaims n =
                pure
                    . Left
                    . (,fromIntegral n)
                    . MultiAnd.make
                    . map StuckClaim
                    . mapMaybe (extractUnproven . snd)

        case traversalResult of
            GraphTraversal.GotStuck n rs ->
                returnUnprovenClaims n rs
            GraphTraversal.Aborted n rs ->
                returnUnprovenClaims n rs
            GraphTraversal.Ended results -> do
                let depths = map fst results
                    maxProofDepth = sconcat (ProofDepth 0 :| depths)
                infoProvenDepth maxProofDepth
                warnProvenClaimZeroDepth maxProofDepth goal
                pure $ Right ()
      where
        -------------------------------
        -- brought in from Claim.hs to remove Strategy type
        infinite :: [GraphTraversal.Step Prim]
        ~infinite = stepNoClaims : repeat stepWithClaims

        withMinDepth :: MinDepth -> [GraphTraversal.Step Prim]
        withMinDepth d =
            noCheckSteps <> repeat stepWithClaims
          where
            noCheckSteps =
                [Begin, Simplify, ApplyAxioms, Simplify] :
                replicate
                    (getMinDepth d - 1)
                    [Begin, Simplify, ApplyAxioms, ApplyClaims, Simplify]

        stepNoClaims =
            [Begin, Simplify, CheckImplication, ApplyAxioms, Simplify]
        stepWithClaims =
            [Begin, Simplify, CheckImplication, ApplyClaims, ApplyAxioms, Simplify]
        -------------------------------

        transition ::
            (GraphTraversal.TState Prim (ProofDepth, ClaimState SomeClaim)) ->
            Simplifier
                ( GraphTraversal.TransitionResult
                    (GraphTraversal.TState Prim (ProofDepth, ClaimState SomeClaim))
                )
        transition =
            GraphTraversal.simpleTransition
                (trackProofDepth $ transitionRule' stuckCheck claims axioms)
                toTransitionResultWithDepth

        -- result interpretation for GraphTraversal.simpleTransition
        toTransitionResultWithDepth ::
            Show c =>
            -- | prior state, needed for [] and Proven cases
            (ProofDepth, ClaimState c) ->
            [(ProofDepth, ClaimState c)] ->
            GraphTraversal.TransitionResult (ProofDepth, ClaimState c)
        toTransitionResultWithDepth prior = \case
            []
                | isJust (extractStuck $ snd prior) -> GraphTraversal.Stuck prior
                | isJust (extractUnproven $ snd prior) -> GraphTraversal.Stopped [prior]
                | otherwise -> GraphTraversal.Final prior
            [c@(_, ClaimState.Claimed{})] -> GraphTraversal.Continuing c
            [c@(_, ClaimState.Rewritten{})] -> GraphTraversal.Continuing c
            [c@(_, ClaimState.Remaining{})] -> GraphTraversal.Continuing c
            [c@(_, ClaimState.Stuck{})] -> GraphTraversal.Stuck c
            [(_, ClaimState.Proven)] -> GraphTraversal.Final prior
            cs@(c : cs')
                | noneStuck (map snd cs) -> GraphTraversal.Branch prior (c :| cs')
                | otherwise ->
                    -- NB we cannot get stuck and unstuck states from
                    -- the same step: CheckImplication yields `Stuck`
                    -- before branching can happen (ApplyAxioms)
                    error $ "toTransitionResult: " <> show (prior, cs)
              where
                noneStuck :: [ClaimState c] -> Bool
                noneStuck = null . mapMaybe ClaimState.extractStuck

{- | Attempts to perform a single proof step, starting at the configuration
 in the execution graph designated by the provided node. Re-constructs the
 execution graph by inserting this step.
-}
proveClaimStep ::
    forall simplifier.
    MonadSimplify simplifier =>
    MonadMask simplifier =>
    MonadProf simplifier =>
    Maybe MinDepth ->
    StuckCheck ->
    -- | list of claims in the spec module
    [SomeClaim] ->
    -- | list of axioms in the main module
    [Rule SomeClaim] ->
    -- | current execution graph
    ExecutionGraph CommonClaimState (AppliedRule SomeClaim) ->
    -- | selected node in the graph
    Graph.Node ->
    simplifier (ExecutionGraph CommonClaimState (AppliedRule SomeClaim))
proveClaimStep _ stuckCheck claims axioms executionGraph node =
    executionHistoryStep
        transitionRule''
        strategy'
        executionGraph
        node
  where
    -- TODO(Ana): The kore-repl doesn't support --min-depth <n> yet.
    -- If requested, add a state layer which keeps track of
    -- the depth, which should compare it to the minDepth and
    -- decide the appropriate strategy for the next step.
    -- We should also add a command for toggling this feature on and
    -- off.
    strategy' :: Strategy Prim
    strategy'
        | isRoot = firstStep
        | otherwise = followupStep

    firstStep :: Strategy Prim
    firstStep = reachabilityFirstStep

    followupStep :: Strategy Prim
    followupStep = reachabilityNextStep

    ExecutionGraph{root} = executionGraph

    isRoot :: Bool
    isRoot = node == root

    transitionRule'' prim state
        | isRoot =
            transitionRule'
                stuckCheck
                claims
                axioms
                prim
                (Lens.over lensClaimPattern mkGoal <$> state)
        | otherwise =
            transitionRule' stuckCheck claims axioms prim state

transitionRule' ::
    MonadSimplify simplifier =>
    MonadProf simplifier =>
    MonadMask simplifier =>
    StuckCheck ->
    [SomeClaim] ->
    [Rule SomeClaim] ->
    CommonTransitionRule simplifier
transitionRule' stuckCheck claims axioms = \prim proofState ->
    deepseq
        proofState
        ( transitionRule stuckCheck claims axiomGroups
            & withWarnings
            & profTransitionRule
            & withConfiguration
            & withDebugClaimState
            & withDebugProven
            & logTransitionRule
            & checkStuckConfiguration
        )
        prim
        proofState
  where
    axiomGroups = groupSortOn Attribute.Axiom.getPriorityOfAxiom axioms

withWarnings ::
    forall m.
    MonadSimplify m =>
    CommonTransitionRule m ->
    CommonTransitionRule m
withWarnings rule prim claimState = do
    claimState' <- rule prim claimState
    case prim of
        Prim.CheckImplication | ClaimState.Stuck _ <- claimState' ->
            case claimState of
                ClaimState.Remaining claim -> warnStuckClaimStateTermsNotUnifiable claim
                ClaimState.Claimed claim -> warnStuckClaimStateTermsUnifiable claim
                _ -> return ()
        _ -> return ()
    return claimState'

profTransitionRule ::
    forall m.
    MonadProf m =>
    CommonTransitionRule m ->
    CommonTransitionRule m
profTransitionRule rule prim proofState =
    case prim of
        Prim.ApplyClaims -> tracing ":transit:apply-claims"
        Prim.ApplyAxioms -> tracing ":transit:apply-axioms"
        Prim.CheckImplication -> tracing ":transit:check-implies"
        Prim.Simplify -> tracing ":transit:simplify"
        _ -> rule prim proofState
  where
    tracing name =
        lift (traceProf name (runTransitionT (rule prim proofState)))
            >>= Transition.scatter

logTransitionRule ::
    forall m.
    MonadSimplify m =>
    CommonTransitionRule m ->
    CommonTransitionRule m
logTransitionRule rule prim proofState =
    whileReachability prim $ rule prim proofState

checkStuckConfiguration ::
    CommonTransitionRule m ->
    CommonTransitionRule m
checkStuckConfiguration rule prim proofState = do
    proofState' <- rule prim proofState
    for_ (extractStuck proofState) $ \rule' -> do
        let resultPatternPredicate = predicate (getConfiguration rule')
            multiAndPredicate = getMultiAndPredicate resultPatternPredicate
        when (any isNot_Ceil_ multiAndPredicate) $
            error . show . Pretty.vsep $
                [ "Found '\\not(\\ceil(_))' in stuck configuration:"
                , Pretty.pretty rule'
                , "Please file a bug report:\
                  \ https://github.com/runtimeverification/haskell-backend/issues"
                ]
    return proofState'
  where
    isNot_Ceil_ :: Predicate variable -> Bool
    isNot_Ceil_ (PredicateNot (PredicateCeil _)) = True
    isNot_Ceil_ _ = False

-- | Modify a 'TransitionRule' to track the depth of a proof.
trackProofDepth ::
    forall m rule goal.
    TransitionRule m rule (ClaimState goal) ->
    TransitionRule m rule (ProofDepth, ClaimState goal)
trackProofDepth rule prim (!proofDepth, proofState) = do
    proofState' <- rule prim proofState
    let proofDepth' = (if didRewrite proofState' then succ else id) proofDepth
    pure (proofDepth', proofState')
  where
    didRewrite proofState' =
        isApply prim
            && ClaimState.isRewritable proofState
            && isRewritten proofState'

    isApply Prim.ApplyClaims = True
    isApply Prim.ApplyAxioms = True
    isApply _ = False

    isRewritten (ClaimState.Rewritten _) = True
    isRewritten ClaimState.Proven = True
    isRewritten _ = False

debugClaimStateBracket ::
    forall monad rule.
    MonadLog monad =>
    From rule Attribute.Axiom.SourceLocation =>
    -- | current proof state
    ClaimState SomeClaim ->
    -- | transition
    Prim ->
    -- | action to be computed
    Transition.TransitionT rule monad (ClaimState SomeClaim) ->
    Transition.TransitionT rule monad (ClaimState SomeClaim)
debugClaimStateBracket proofState transition action = do
    debugBeforeTransition proofState transition
    (result, rules) <- Transition.record action
    debugAfterTransition result transition $ toList rules
    return result

debugClaimStateFinal ::
    forall monad.
    Alternative monad =>
    MonadLog monad =>
    -- | transition
    Prim ->
    monad (ClaimState SomeClaim)
debugClaimStateFinal transition = do
    debugFinalTransition transition
    empty

withDebugClaimState ::
    forall monad.
    MonadLog monad =>
    CommonTransitionRule monad ->
    CommonTransitionRule monad
withDebugClaimState transitionFunc transition state =
    Transition.orElse
        ( debugClaimStateBracket
            state
            transition
            (transitionFunc transition state)
        )
        ( debugClaimStateFinal
            transition
        )

withDebugProven ::
    forall monad.
    MonadLog monad =>
    CommonTransitionRule monad ->
    CommonTransitionRule monad
withDebugProven rule prim state =
    do
        state' <- rule prim state
        case state' of
            ClaimState.Proven ->
                case extractUnproven state of
                    Just claim ->
                        do
                            Log.logEntry DebugProven{claim}
                            pure state'
                    _ -> pure state'
            _ -> pure state'

withConfiguration ::
    MonadCatch monad =>
    CommonTransitionRule monad ->
    CommonTransitionRule monad
withConfiguration transit prim proofState =
    handle' (transit prim proofState)
  where
    config = extractUnproven proofState & fmap getConfiguration
    handle' = maybe id handleConfig config
    handleConfig config' =
        handleAll $
            throwM
                . WithConfiguration (getRewritingPattern config')
