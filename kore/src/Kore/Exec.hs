{- |
Module      : Kore.Exec
Description : Expose concrete execution as a library
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Stability   : experimental
Portability : portable

Expose concrete execution as a library
-}
module Kore.Exec (
    exec,
    rpcExec,
    StopLabels (..),
    RuleTrace (..),
    RpcExecState (..),
    search,
    prove,
    proveWithRepl,
    matchDisjunction,
    checkFunctions,
    simplify,
    Rewrite,
    Equality,
    Initialized (..),
    makeSerializedModule,
    SerializedModule (..),
) where

import Control.Arrow (Arrow ((&&&)))
import Control.Concurrent.MVar
import Control.DeepSeq (
    deepseq,
 )
import Control.Error (
    hoistMaybe,
 )
import Control.Lens qualified as Lens
import Control.Monad (
    filterM,
    (>=>),
 )
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Validate
import Data.Coerce (
    coerce,
 )
import Data.Limit as Limit (
    Limit (..),
    takeWithin,
 )
import Data.List (
    tails,
 )
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, (><))
import Data.Text (Text)
import GHC.Generics qualified as GHC
import Kore.Attribute.Axiom (Label, UniqueId)
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Definition
import Kore.Attribute.Symbol (
    StepperAttributes,
 )
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Builtin qualified as Builtin
import Kore.Equation (
    Equation (..),
    extractEquations,
    isSimplificationRule,
    right,
 )
import Kore.Equation qualified as Equation (
    Equation (antiLeft),
    argument,
    requires,
 )
import Kore.Exec.GraphTraversal (extractStuckTraversalResult)
import Kore.Exec.GraphTraversal qualified as GraphTraversal
import Kore.IndexedModule.IndexedModule (
    IndexedModule (..),
    VerifiedModule,
    VerifiedModuleSyntax,
 )
import Kore.IndexedModule.IndexedModule qualified as IndexedModule
import Kore.IndexedModule.MetadataTools (
    SmtMetadataTools,
 )
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools (
    build,
 )
import Kore.IndexedModule.OverloadGraph
import Kore.IndexedModule.OverloadGraph qualified as OverloadGraph
import Kore.IndexedModule.Resolvers (
    resolveInternalSymbol,
 )
import Kore.IndexedModule.SortGraph
import Kore.IndexedModule.SortGraph qualified as SortGraph
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.InternalInt
import Kore.Internal.MultiOr qualified as MultiOr
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (
    Pattern,
 )
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    fromPredicate,
    makeMultipleOrPredicate,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.SideCondition qualified as SideCondition
import Kore.Internal.TermLike
import Kore.Log.DebugRewriteTrace (
    debugFinalPatterns,
    debugInitialPattern,
 )
import Kore.Log.ErrorEquationRightFunction (
    errorEquationRightFunction,
 )
import Kore.Log.ErrorEquationsSameMatch (
    errorEquationsSameMatch,
 )
import Kore.Log.ErrorRewriteLoop (
    errorRewriteLoop,
 )
import Kore.Log.InfoExecDepth
import Kore.Log.KoreLogOptions (
    DebugOptionsValidationError (..),
    KoreLogOptions (..),
    validateDebugOptions,
 )
import Kore.Log.WarnDepthLimitExceeded (
    warnDepthLimitExceeded,
 )
import Kore.Log.WarnTrivialClaim
import Kore.Reachability (
    AllowVacuous,
    AlreadyProven (AlreadyProven),
    Axioms (Axioms),
    MinDepth,
    ProveClaimsResult (..),
    Rule (ReachabilityRewriteRule),
    SomeClaim (..),
    StuckCheck (..),
    ToProve (ToProve),
    extractClaims,
    isTrusted,
    lensClaimPattern,
    proveClaims,
 )
import Kore.Repl qualified as Repl
import Kore.Repl.Data qualified as Repl.Data
import Kore.Rewrite
import Kore.Rewrite.Axiom.Identifier (
    AxiomIdentifier,
 )
import Kore.Rewrite.RewriteStep qualified as Step
import Kore.Rewrite.RewritingVariable
import Kore.Rewrite.Rule (
    extractRewriteAxioms,
 )
import Kore.Rewrite.Rule.Expand (
    ExpandSingleConstructors (..),
 )
import Kore.Rewrite.Rule.Simplify (
    SimplifyRuleLHS (..),
 )
import Kore.Rewrite.Rule.Simplify qualified as Rule
import Kore.Rewrite.RulePattern (
    RewriteRule (..),
    lhsEqualsRhs,
    mapRuleVariables,
 )
import Kore.Rewrite.RulePattern as RulePattern (
    RulePattern (..),
 )
import Kore.Rewrite.Search (
    searchGraph,
 )
import Kore.Rewrite.Search qualified as Search
import Kore.Rewrite.Strategy qualified as Strategy
import Kore.Rewrite.Timeout (
    EnableMovingAverage (..),
    StepTimeout,
 )
import Kore.Rewrite.Transition (
    runTransitionT,
    scatter,
 )
import Kore.Simplify.API (
    evalSimplifier,
    evalSimplifierLogged,
    evalSimplifierProofs,
 )
import Kore.Simplify.Pattern qualified as Pattern
import Kore.Simplify.Simplify (
    MonadSimplify (liftSimplifier),
    Simplifier,
    SimplifierTrace,
    askMetadataTools,
 )
import Kore.Syntax.Module (
    ModuleName,
 )
import Kore.TopBottom (
    isBottom,
 )
import Kore.Unparser (
    unparseToText,
    unparseToText2,
 )
import Log qualified
import Logic (
    LogicT,
    observeAllT,
 )
import Logic qualified
import Prelude.Kore
import Prof
import SMT (
    SMT,
 )
import System.Exit (
    ExitCode (..),
 )

-- | Semantic rule used during execution.
type Rewrite = RewriteRule RewritingVariableName

-- | Function rule used during execution.
type Equality = Equation VariableName

-- | A collection of rules and simplifiers used during execution.
newtype Initialized = Initialized {rewriteRules :: [Rewrite]}
    deriving stock (GHC.Generic)
    deriving anyclass (NFData)

data SerializedModule = SerializedModule
    { sortGraph :: SortGraph
    , overloadGraph :: OverloadGraph
    , metadataTools :: SmtMetadataTools StepperAttributes
    , verifiedModule :: VerifiedModuleSyntax StepperAttributes
    , rewrites :: Initialized
    , equations :: Map AxiomIdentifier [Equation VariableName]
    }
    deriving stock (GHC.Generic)
    deriving anyclass (NFData)

makeSerializedModule ::
    VerifiedModule StepperAttributes ->
    SMT SerializedModule
makeSerializedModule verifiedModule =
    evalSimplifier (indexedModuleSyntax verifiedModule') sortGraph overloadGraph metadataTools equations $ do
        rewrites <- initializeAndSimplify verifiedModule
        return
            SerializedModule
                { sortGraph
                , overloadGraph
                , metadataTools
                , verifiedModule = indexedModuleSyntax verifiedModule
                , rewrites
                , equations
                }
  where
    sortGraph = SortGraph.fromIndexedModule verifiedModule
    overloadGraph = OverloadGraph.fromIndexedModule verifiedModule
    earlyMetadataTools = MetadataTools.build verifiedModule
    verifiedModule' =
        IndexedModule.mapPatterns
            (Builtin.internalize earlyMetadataTools)
            verifiedModule
    metadataTools = MetadataTools.build verifiedModule'
    equations = extractEquations verifiedModule'

-- | Symbolic execution
exec ::
    Maybe StepTimeout ->
    EnableMovingAverage ->
    Limit Natural ->
    Limit Natural ->
    Strategy.FinalNodeType ->
    -- | The main module
    SerializedModule ->
    ExecutionMode ->
    -- | The input pattern
    TermLike VariableName ->
    SMT (ExitCode, TermLike VariableName)
exec
    stepTimeout
    enableMA
    depthLimit
    breadthLimit
    finalNodeType
    SerializedModule
        { sortGraph
        , overloadGraph
        , metadataTools
        , verifiedModule
        , rewrites = Initialized{rewriteRules}
        , equations
        }
    execMode
    inputPattern@(mkRewritingTerm -> initialTerm) = do
        debugInitialPattern inputPattern
        evalSimplifier verifiedModule' sortGraph overloadGraph metadataTools equations $ do
            finals <-
                GraphTraversal.graphTraversal
                    GraphTraversal.GraphTraversalCancel
                    stepTimeout
                    enableMA
                    finalNodeType
                    Strategy.DepthFirst
                    breadthLimit
                    transit
                    (const Nothing)
                    Limit.Unlimited
                    execStrategy
                    (ExecDepth 0, Start $ Pattern.fromTermLike initialTerm)
                    >>= \case
                        GraphTraversal.Ended results ->
                            pure results
                        GraphTraversal.GotStuck _ results ->
                            pure $ map extractStuckTraversalResult results
                        GraphTraversal.Stopped results nexts -> do
                            when (null nexts) $
                                forM_ depthLimit warnDepthLimitExceeded
                            pure results
                        GraphTraversal.Aborted results -> do
                            pure results
                        GraphTraversal.TimedOut _ results -> do
                            pure results

            let (depths, finalConfigs) = unzip finals
            infoExecDepth (maximum (ExecDepth 0 : depths))
            let finalConfigs' =
                    MultiOr.make $
                        mapMaybe (fst . extractProgramState) finalConfigs
            debugFinalPatterns finalConfigs'
            exitCode <- getExitCode verifiedModule finalConfigs'
            let finalTerm =
                    MultiOr.map getRewritingPattern finalConfigs'
                        & OrPattern.toTermLike initialSort
                        & sameTermLikeSort initialSort
            return (exitCode, finalTerm)
      where
        verifiedModule' =
            IndexedModule.mapAliasPatterns
                -- TODO (thomas.tuegel): Move this into Kore.Builtin
                (Builtin.internalize metadataTools)
                verifiedModule
        initialSort = termLikeSort initialTerm

        execStrategy :: [Strategy.Step Prim]
        execStrategy =
            Limit.takeWithin depthLimit $
                [Begin, Simplify, Rewrite, Simplify]
                    : repeat [Begin, Rewrite, Simplify]

        transit ::
            GraphTraversal.TState
                Prim
                ( ExecDepth
                , ProgramState
                    (RuleInfo RewritingVariableName)
                    (Pattern RewritingVariableName)
                ) ->
            Simplifier
                ( GraphTraversal.TransitionResult
                    ( GraphTraversal.TState
                        Prim
                        ( ExecDepth
                        , ProgramState
                            (RuleInfo RewritingVariableName)
                            (Pattern RewritingVariableName)
                        )
                    )
                )
        transit =
            GraphTraversal.simpleTransition
                ( trackExecDepth . profTransitionRule $
                    transitionRule (groupRewritesByPriority rewriteRules) Step.DisableAssumeInitialDefined execMode
                )
                toTransitionResult

        toTransitionResult ::
            (ExecDepth, ProgramState d p) ->
            [(ExecDepth, ProgramState d p)] ->
            ( GraphTraversal.TransitionResult
                (ExecDepth, ProgramState d p)
            )
        toTransitionResult prior [] =
            case snd prior of
                Start _ -> GraphTraversal.Stop prior []
                Rewritten _ _ -> GraphTraversal.Stop prior []
                Remaining _ -> GraphTraversal.Stuck prior
                Kore.Rewrite.Bottom -> GraphTraversal.Stuck prior
        toTransitionResult _prior [next] =
            case snd next of
                Start _ -> GraphTraversal.Continuing next
                Rewritten _ _ -> GraphTraversal.Continuing next
                Remaining _ -> GraphTraversal.Stuck next
                Kore.Rewrite.Bottom -> GraphTraversal.Stuck next
        toTransitionResult prior (s : ss) =
            GraphTraversal.Branch prior (s :| ss)

-- | JSON RPC helper structure for cut points and terminal rules
data StopLabels = StopLabels
    { cutPointLabels :: [Text]
    , terminalLabels :: [Text]
    }

data RuleTrace = RuleTrace
    { simplifications :: Seq SimplifierTrace
    , ruleId :: UniqueId
    , ruleLabel :: Label
    }
    deriving stock (Eq, Show)

-- | Type for json-rpc execution state, for readability
data RpcExecState v = RpcExecState
    { rpcProgState :: ProgramState (RuleInfo v) (Pattern v)
    -- ^ program state
    , rpcRules :: Seq RuleTrace
    -- ^ rule label/ids we have applied so far
    , rpcDepth :: ExecDepth
    -- ^ execution depth
    }
    deriving stock (Eq, Show)

startState :: TermLike RewritingVariableName -> RpcExecState RewritingVariableName
startState t =
    RpcExecState
        { rpcProgState = Start $ Pattern.fromTermLike t
        , rpcRules = mempty
        , rpcDepth = ExecDepth 0
        }

{- | Version of @kore-exec@ suitable for the JSON RPC server. Cannot
  execute across branches, supports a depth limit and rule labels to
  stop on (distinguished as cut-points for LHS or terminal-rules for
  RHS).

  Returns the raw traversal result for the RPC server to extract
  response elements, containing program state with an optional stop
  label and a depth counter.
-}
rpcExec ::
    Limit Natural ->
    Maybe StepTimeout ->
    EnableMovingAverage ->
    Step.EnableAssumeInitialDefined ->
    -- | whether tracing is enabled
    Bool ->
    -- | The main module
    SerializedModule ->
    -- | additional labels/rule names for stopping
    StopLabels ->
    -- | The input pattern
    TermLike VariableName ->
    SMT (GraphTraversal.TraversalResult (RpcExecState RewritingVariableName))
rpcExec
    depthLimit
    stepTimeout
    enableMA
    assumeInitialDefined
    tracingEnabled
    SerializedModule
        { sortGraph
        , overloadGraph
        , metadataTools
        , verifiedModule
        , rewrites = Initialized{rewriteRules}
        , equations
        }
    StopLabels{cutPointLabels, terminalLabels}
    (mkRewritingTerm -> initialTerm) =
        simplifierRun $
            GraphTraversal.graphTraversal
                GraphTraversal.GraphTraversalCancel
                stepTimeout
                enableMA
                Strategy.LeafOrBranching
                Strategy.DepthFirst
                (Limit 2) -- breadth limit 2 because we never go beyond a branch
                transit
                (const Nothing)
                Limit.Unlimited
                execStrategy
                (startState initialTerm)
      where
        simplifierRun
            | tracingEnabled =
                fmap snd . evalSimplifierLogged verifiedModule' sortGraph overloadGraph metadataTools equations
            | otherwise =
                evalSimplifier verifiedModule' sortGraph overloadGraph metadataTools equations

        verifiedModule' =
            IndexedModule.mapAliasPatterns
                (Builtin.internalize metadataTools)
                verifiedModule

        execStrategy :: [Strategy.Step Prim]
        execStrategy =
            Limit.takeWithin depthLimit $
                [Begin, Simplify, Rewrite, Simplify]
                    : repeat [Begin, Rewrite, Simplify]

        transit ::
            GraphTraversal.TState Prim (RpcExecState RewritingVariableName) ->
            Simplifier
                ( GraphTraversal.TransitionResult
                    (GraphTraversal.TState Prim (RpcExecState RewritingVariableName))
                )
        transit =
            GraphTraversal.transitionWithRule
                ( withRpcExecState . trackExecDepth . profTransitionRule $
                    transitionRule (groupRewritesByPriority rewriteRules) assumeInitialDefined All
                )
                toTransitionResult

        -- The rule label is carried around unmodified in the
        -- transition, and adjusted in `toTransitionResult`
        withRpcExecState ::
            TransitionRule m r (ExecDepth, ProgramState (RuleInfo v) (Pattern v)) ->
            TransitionRule m r (RpcExecState v)
        withRpcExecState transition =
            \prim RpcExecState{rpcProgState, rpcRules, rpcDepth} ->
                fmap (\(d, st) -> RpcExecState st rpcRules d) $
                    transition prim (rpcDepth, rpcProgState)

        toTransitionResult ::
            RpcExecState v ->
            [(RpcExecState v, Seq (RewriteRule v, Seq SimplifierTrace))] ->
            (GraphTraversal.TransitionResult (RpcExecState v))
        toTransitionResult prior@RpcExecState{rpcProgState = priorPState} [] =
            case priorPState of
                Remaining _ -> GraphTraversal.Stuck prior
                -- this should not be reachable unless we received bottom as initial configuration?
                Kore.Rewrite.Bottom -> GraphTraversal.Vacuous prior
                -- returns `Final` to signal that no instructions were left.
                Start _ -> GraphTraversal.Final prior
                Rewritten _ _ -> GraphTraversal.Final prior
        toTransitionResult prior@RpcExecState{rpcRules = priorRules} [(next, rules)]
            | (_ : _) <- mapMaybe (isCutPoint . fst) (toList rules) =
                GraphTraversal.Stop
                    prior
                    [setTraces rules priorRules next]
            | (_ : _) <- mapMaybe (isTerminalRule . fst) $ toList rules =
                GraphTraversal.Stop
                    (setTraces rules priorRules next)
                    []
        toTransitionResult prior@RpcExecState{rpcRules = priorRules} [(next@RpcExecState{rpcProgState = nextPState}, rules)] =
            let next' = setTraces rules priorRules next
             in case nextPState of
                    Start _ -> GraphTraversal.Continuing next'
                    Rewritten _ _ -> GraphTraversal.Continuing next'
                    Remaining _ -> GraphTraversal.Stuck next'
                    Kore.Rewrite.Bottom -> GraphTraversal.Vacuous prior
        toTransitionResult prior rs =
            case filter
                ( \(RpcExecState{rpcProgState}, _) -> case rpcProgState of
                    Kore.Rewrite.Bottom -> False
                    _ -> True
                )
                rs of
                (s1 : s2 : ss) -> GraphTraversal.Branch prior $ fmap fst (s1 :| (s2 : ss))
                --  either empty or single result
                other -> toTransitionResult prior other

        setTraces ::
            Seq (RewriteRule v, Seq SimplifierTrace) -> Seq RuleTrace -> RpcExecState v -> RpcExecState v
        setTraces rules priorTraces state
            | tracingEnabled =
                state{rpcRules = priorTraces >< fmap extractRuleTrace rules}
            | otherwise -- need to retain the last trace for the terminal/cut-point check
                =
                state{rpcRules = fmap extractRuleTrace rules}
          where
            extractUniqueIdAndLabel :: RewriteRule v -> (UniqueId, Label)
            extractUniqueIdAndLabel = (Attribute.uniqueId &&& Attribute.label) . RulePattern.attributes . getRewriteRule
            extractRuleTrace = \(rr, simps) -> uncurry (RuleTrace simps) $ extractUniqueIdAndLabel rr

        -- helpers to extract a matched rule label or identifier
        isCutPoint, isTerminalRule :: RewriteRule v -> Maybe Text
        isCutPoint = retractIfElement cutPointLabels
        isTerminalRule = retractIfElement terminalLabels

        retractIfElement
            labels
            (RewriteRule RulePattern{attributes = Attribute.Axiom{label, uniqueId}})
                | Just lbl <- Attribute.unLabel label
                , lbl `elem` labels =
                    Just lbl
                | Just uid <- Attribute.getUniqueId uniqueId
                , uid `elem` labels =
                    Just uid
                | otherwise =
                    Nothing

-- | Modify a 'TransitionRule' to track the depth of the execution graph.
trackExecDepth ::
    TransitionRule monad rule (ProgramState d p) ->
    TransitionRule monad rule (ExecDepth, ProgramState d p)
trackExecDepth transit prim (execDepth, execState) = do
    execState' <- transit prim execState
    let execDepth' = (if didRewrite execState' then succ else id) execDepth
    pure (execDepth', execState')
  where
    -- The new state can become Bottom by simplification after rewriting,
    -- or it remains Rewritten. If it is Remaining, it was not rewritten.
    didRewrite Kore.Rewrite.Bottom = prim == Rewrite
    didRewrite (Rewritten _ _) = prim == Rewrite
    didRewrite _ = False

-- | Add profiling markers to a 'TransitionRule'.
profTransitionRule ::
    forall monad rule state.
    MonadProf monad =>
    TransitionRule monad rule state ->
    TransitionRule monad rule state
profTransitionRule rule prim proofState =
    case prim of
        Rewrite -> Just ":rewrite:"
        Simplify -> Just ":simplify:"
        Begin -> Nothing
        & \case
            Just marker -> lift (traceProf marker (runTransitionT go)) >>= scatter
            Nothing -> go
  where
    go = rule prim proofState

-- | Project the value of the exit cell, if it is present.
getExitCode ::
    -- | The main module
    VerifiedModuleSyntax StepperAttributes ->
    -- | The final configuration(s) of execution
    OrPattern.OrPattern RewritingVariableName ->
    Simplifier ExitCode
getExitCode
    indexedModule
    configs =
        takeExitCode $ \mkExitCodeSymbol -> do
            let mkGetExitCode t = mkApplySymbol (mkExitCodeSymbol []) [t]
            exitCodePatterns <-
                do
                    config <- Logic.scatter configs
                    liftSimplifier (Pattern.simplifyTopConfiguration (mkGetExitCode <$> config))
                        >>= Logic.scatter
                    & MultiOr.observeAllT
            let exitCode =
                    case toList (MultiOr.map Pattern.term exitCodePatterns) of
                        [exitTerm] -> extractExit exitTerm
                        _ -> ExitFailure 111
            return exitCode
      where
        extractExit = \case
            InternalInt_ InternalInt{internalIntValue = exit}
                | exit == 0 -> ExitSuccess
                | otherwise -> ExitFailure (fromInteger exit)
            _ -> ExitFailure 111

        resolve =
            resolveInternalSymbol indexedModule
                . noLocationId

        takeExitCode ::
            (([Sort] -> Symbol) -> Simplifier ExitCode) ->
            Simplifier ExitCode
        takeExitCode act =
            resolve "LblgetExitCode"
                & maybe (pure ExitSuccess) act

-- | Symbolic search
search ::
    Limit Natural ->
    Limit Natural ->
    -- | The main module
    SerializedModule ->
    -- | The input pattern
    TermLike VariableName ->
    -- | The pattern to match during execution
    Pattern VariableName ->
    -- | The bound on the number of search matches and the search type
    Search.Config ->
    SMT (TermLike VariableName)
search
    depthLimit
    breadthLimit
    SerializedModule
        { sortGraph
        , overloadGraph
        , metadataTools
        , verifiedModule
        , rewrites
        , equations
        }
    (mkRewritingTerm -> termLike)
    searchPattern
    searchConfig =
        evalSimplifier verifiedModule sortGraph overloadGraph metadataTools equations $ do
            let Initialized{rewriteRules} = rewrites
            simplifiedPatterns <-
                Pattern.simplify $
                    Pattern.fromTermLike termLike
            let initialPattern =
                    case toList simplifiedPatterns of
                        [] -> Pattern.bottomOf (termLikeSort termLike)
                        (config : _) -> config
                rewriteGroups =
                    groupRewritesByPriority rewriteRules
                runStrategy' =
                    Strategy.runStrategy
                        breadthLimit
                        -- search relies on exploring
                        -- the entire space of states.
                        ( transitionRule rewriteGroups Step.DisableAssumeInitialDefined All
                            & profTransitionRule
                        )
                        (limitedExecutionStrategy depthLimit)
            executionGraph <-
                runStrategy' (Start initialPattern)
            let match target config1 config2 = do
                    extracted <- hoistMaybe $ fst $ extractProgramState config2
                    Search.matchWith target config1 extracted
            solutionsLists <-
                searchGraph
                    searchConfig
                    ( match
                        SideCondition.top
                        (mkRewritingPattern searchPattern)
                    )
                    executionGraph
            let solutions = concatMap toList solutionsLists
                orPredicate =
                    makeMultipleOrPredicate (Condition.toPredicate <$> solutions)
            return
                . sameTermLikeSort patternSort
                . getRewritingTerm
                . fromPredicate patternSort
                $ orPredicate
      where
        patternSort = termLikeSort termLike

-- | Proving a spec given as a module containing rules to be proven
prove ::
    KoreLogOptions ->
    Maybe MinDepth ->
    Maybe StepTimeout ->
    EnableMovingAverage ->
    StuckCheck ->
    AllowVacuous ->
    Strategy.GraphSearchOrder ->
    Limit Natural ->
    Limit Natural ->
    Natural ->
    Strategy.FinalNodeType ->
    -- | The main module
    VerifiedModule StepperAttributes ->
    -- | The spec module
    VerifiedModule StepperAttributes ->
    -- | The module containing the claims that were proven in a previous run.
    Maybe (VerifiedModule StepperAttributes) ->
    SMT ProveClaimsResult
prove
    koreLogOptions
    maybeMinDepth
    stepTimeout
    enableMA
    stuckCheck
    allowVacuous
    searchOrder
    breadthLimit
    depthLimit
    maxCounterexamples
    finalNodeType
    definitionModule
    specModule
    trustedModule =
        evalSimplifierProofs definitionModule $ do
            InitializedProver{axioms, specClaims, claims, alreadyProven} <-
                initializeProver
                    koreLogOptions
                    definitionModule
                    specModule
                    trustedModule
            proveClaims
                maybeMinDepth
                stepTimeout
                enableMA
                stuckCheck
                allowVacuous
                breadthLimit
                searchOrder
                maxCounterexamples
                finalNodeType
                specClaims
                (Axioms axioms)
                (AlreadyProven (map unparseToText2 alreadyProven))
                ( ToProve
                    ( map
                        (\x -> (x, depthLimit))
                        (extractUntrustedClaims' claims)
                    )
                )
      where
        extractUntrustedClaims' :: [SomeClaim] -> [SomeClaim]
        extractUntrustedClaims' = filter (not . isTrusted)

{- | Initialize and run the repl with the main and spec modules. This will loop
 the repl until the user exits.
-}
proveWithRepl ::
    Maybe MinDepth ->
    Maybe StepTimeout ->
    EnableMovingAverage ->
    Repl.Data.StepTime ->
    StuckCheck ->
    AllowVacuous ->
    -- | The main module
    VerifiedModule StepperAttributes ->
    -- | The spec module
    VerifiedModule StepperAttributes ->
    -- | The module containing the claims that were proven in a previous run.
    Maybe (VerifiedModule StepperAttributes) ->
    MVar (Log.LogAction IO Log.SomeEntry) ->
    -- | Optional script
    Repl.Data.ReplScript ->
    -- | Run in a specific repl mode
    Repl.Data.ReplMode ->
    -- | Optional flag for output in run-mode
    Repl.Data.ScriptModeOutput ->
    -- | Optional Output file
    Repl.Data.OutputFile ->
    ModuleName ->
    KoreLogOptions ->
    KFileLocations ->
    Repl.Data.KompiledDir ->
    Repl.Data.KorePrintCommand ->
    SMT ()
proveWithRepl
    minDepth
    stepTimeout
    enableMA
    stepTime
    stuckCheck
    allowVacuous
    definitionModule
    specModule
    trustedModule
    mvar
    replScript
    replMode
    scriptModeOutput
    outputFile
    mainModuleName
    logOptions
    kFileLocations
    kompiledDir
    korePrintCommand = do
        stepMovingAverage <- liftIO newEmptyMVar
        evalSimplifierProofs definitionModule $ do
            InitializedProver{axioms, specClaims, claims} <-
                initializeProver
                    logOptions
                    definitionModule
                    specModule
                    trustedModule
            Repl.runRepl
                minDepth
                stepTimeout
                enableMA
                stepTime
                stepMovingAverage
                stuckCheck
                allowVacuous
                axioms
                specClaims
                claims
                mvar
                replScript
                replMode
                scriptModeOutput
                outputFile
                mainModuleName
                logOptions
                kFileLocations
                kompiledDir
                korePrintCommand

matchDisjunction ::
    VerifiedModule Attribute.Symbol ->
    Pattern RewritingVariableName ->
    [Pattern RewritingVariableName] ->
    SMT (TermLike VariableName)
matchDisjunction mainModule matchPattern disjunctionPattern =
    evalSimplifierProofs mainModule $ do
        results <-
            concatMap toList . catMaybes
                <$> traverse (runMaybeT . match matchPattern) disjunctionPattern
        results
            <&> Condition.toPredicate
            & Predicate.makeMultipleOrPredicate
            & Predicate.fromPredicate sort
            & getRewritingTerm
            & return
  where
    sort = Pattern.patternSort matchPattern
    match = Search.matchWith SideCondition.top

{- | Ensure that for every equation in a function definition, the right-hand
side of the equation is a function pattern. Additionally, check if any function
definition in the module carries two equations that both match the same term.
'checkFunctions' first extracts equations from a verified module to a list of
lists of equations, ignoring any equations that are simplification rules. Then
check that the 'right' side of each equation is a function pattern. Any
equations whose 'right' side is __not__ a function pattern are thrown as an
'ErrorEquationRightFunction'. Finally, check if two equations both match the
same term. Start by constructing all in-order pairs of equations in each
sub-list. Then check that for each pair they both do __not__ match the same
term. Any pairs that do match the same term throw 'ErrorEquationsSameMatch'.
See 'checkEquation',
'Kore.Equation.Registry.extractEquations',
'Kore.Internal.TermLike.isFunctionPattern',
'Kore.Log.ErrorEquationRightFunction.errorEquationRightFunction',
'Kore.Log.ErrorEquationsSameMatch.errorEquationsSameMatch'.
-}
checkFunctions ::
    -- | The main module
    VerifiedModule StepperAttributes ->
    SMT ()
checkFunctions verifiedModule =
    evalSimplifierProofs verifiedModule $ do
        -- check if RHS is function pattern
        equations
            >>= filter (not . isFunctionPattern . right)
            & mapM_ errorEquationRightFunction
        -- check if two equations both match the same term
        equations
            >>= inOrderPairs
            & filterM (uncurry bothMatch)
            >>= mapM_ (uncurry errorEquationsSameMatch)
  where
    equations :: [[Equation VariableName]]
    equations =
        extractEquations verifiedModule
            & Map.elems
            & map (filter (not . Kore.Equation.isSimplificationRule))
    -- https://stackoverflow.com/q/34044366/4051020
    inOrderPairs xs = [(x, y) | (x : ys) <- tails xs, y <- ys]

{- | Returns true when both equations match the same term.  See:
https://github.com/runtimeverification/haskell-backend/issues/2472#issue-833143685
-}
bothMatch ::
    Equation VariableName ->
    Equation VariableName ->
    Simplifier Bool
bothMatch eq1 eq2 =
    let pre1 = Equation.requires eq1
        pre2 = Equation.requires eq2
        arg1 = fromMaybe Predicate.makeTruePredicate $ Equation.argument eq1
        arg2 = fromMaybe Predicate.makeTruePredicate $ Equation.argument eq2
        prio1 = fromMaybe Predicate.makeTruePredicate $ Equation.antiLeft eq1
        prio2 = fromMaybe Predicate.makeTruePredicate $ Equation.antiLeft eq2
        check =
            Predicate.makeAndPredicate prio1 prio2
                & Predicate.makeAndPredicate arg2
                & Predicate.makeAndPredicate arg1
                & Predicate.makeAndPredicate pre2
                & Predicate.makeAndPredicate pre1
                & Predicate.mapVariables (pure mkConfigVariable)
        sort = termLikeSort $ right eq1
        patt = Pattern.fromPredicateSorted sort check
     in (not . isBottom) <$> Pattern.simplify patt

simplify ::
    Monad m =>
    Pattern RewritingVariableName ->
    m (Pattern RewritingVariableName)
simplify = return

assertSomeClaims :: Monad m => [claim] -> m ()
assertSomeClaims claims =
    when (null claims) . error $
        "Unexpected empty set of claims.\n"
            ++ "Possible explanation: the frontend and the backend don't agree "
            ++ "on the representation of claims."

simplifySomeClaim ::
    SomeClaim ->
    Simplifier SomeClaim
simplifySomeClaim rule = do
    let claim = Lens.view lensClaimPattern rule
    claim' <- Rule.simplifyClaimPattern claim
    return $ Lens.set lensClaimPattern claim' rule

initializeAndSimplify ::
    VerifiedModule StepperAttributes ->
    Simplifier Initialized
initializeAndSimplify verifiedModule =
    initialize (lift . simplifyRuleLhs >=> Logic.scatter) verifiedModule

-- | Collect various rules and simplifiers in preparation to execute.
initialize ::
    (RewriteRule RewritingVariableName -> LogicT Simplifier (RewriteRule RewritingVariableName)) ->
    VerifiedModule StepperAttributes ->
    Simplifier Initialized
initialize simplificationProcedure verifiedModule = do
    rewriteRules <-
        Logic.observeAllT $ do
            rule <- Logic.scatter (either error id (extractRewriteAxioms verifiedModule))
            initializeRule (mapRuleVariables (pure mkRuleVariable) rule)
    pure Initialized{rewriteRules}
  where
    initializeRule ::
        RewriteRule RewritingVariableName ->
        LogicT Simplifier (RewriteRule RewritingVariableName)
    initializeRule rule = do
        simplRule <- simplificationProcedure rule
        when
            (lhsEqualsRhs $ getRewriteRule simplRule)
            (errorRewriteLoop simplRule)
        deepseq simplRule pure simplRule

data InitializedProver = InitializedProver
    { axioms :: ![Rule SomeClaim]
    , specClaims :: ![SomeClaim]
    , claims :: ![SomeClaim]
    , alreadyProven :: ![SomeClaim]
    }

data MaybeChanged a = Changed !a | Unchanged !a

fromMaybeChanged :: MaybeChanged a -> a
fromMaybeChanged (Changed a) = a
fromMaybeChanged (Unchanged a) = a

-- | Collect various rules and simplifiers in preparation to execute.
initializeProver ::
    KoreLogOptions ->
    VerifiedModule StepperAttributes ->
    VerifiedModule StepperAttributes ->
    Maybe (VerifiedModule StepperAttributes) ->
    Simplifier InitializedProver
initializeProver koreLogOptions definitionModule specModule maybeTrustedModule = do
    initialized <- initializeAndSimplify definitionModule
    let Initialized{rewriteRules} = initialized
        equations = extractEquations definitionModule
        undefinedLabels = runValidate $ validateDebugOptions equations rewriteRules koreLogOptions
    when (isLeft undefinedLabels) $
        throwM . DebugOptionsValidationError $
            fromLeft mempty undefinedLabels
    tools <- askMetadataTools
    let changedSpecClaims :: [MaybeChanged SomeClaim]
        changedSpecClaims = expandClaim tools <$> extractClaims specModule
        simplifyToList :: SomeClaim -> Simplifier [SomeClaim]
        simplifyToList rule = do
            simplified <- simplifyRuleLhs rule
            let result = toList simplified
            when (null result) $ warnTrivialClaimRemoved rule
            return result

        trustedClaims :: [SomeClaim]
        trustedClaims = maybe [] extractClaims maybeTrustedModule

    mapM_ logChangedClaim changedSpecClaims

    let specClaims :: [SomeClaim]
        specClaims = map fromMaybeChanged changedSpecClaims
    -- This assertion should come before simplifying the claims,
    -- since simplification should remove all trivial claims.
    assertSomeClaims specClaims
    simplifiedSpecClaims <- mapM simplifyToList specClaims
    claims <- traverse simplifySomeClaim (concat simplifiedSpecClaims)
    let axioms = coerce <$> rewriteRules
        alreadyProven = trustedClaims
    pure InitializedProver{axioms, specClaims, claims, alreadyProven}
  where
    expandClaim ::
        SmtMetadataTools attributes ->
        SomeClaim ->
        MaybeChanged SomeClaim
    expandClaim tools claim =
        if claim /= expanded
            then Changed expanded
            else Unchanged claim
      where
        expanded = expandSingleConstructors tools claim

    logChangedClaim ::
        MaybeChanged SomeClaim ->
        Simplifier ()
    logChangedClaim (Changed claim) =
        Log.logInfo ("Claim variables were expanded:\n" <> unparseToText claim)
    logChangedClaim (Unchanged _) = return ()
