{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.JsonRpc (
    module Kore.JsonRpc,
) where

import Control.Concurrent.MVar qualified as MVar
import Control.Monad.Except (MonadError (throwError), liftEither, runExceptT, withExceptT)
import Control.Monad.Logger (runLoggingT)
import Control.Monad.Trans.Except (catchE)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Coerce (coerce)
import Data.Conduit.Network (serverSettings)
import Data.Default (Default (..))
import Data.IORef (readIORef)
import Data.InternedText (globalInternedTextCache)
import Data.Limit (Limit (..))
import Data.List.Extra (mconcatMap)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding (encodeUtf8)
import GlobalMain (
    LoadedDefinition (..),
    SerializedDefinition (..),
 )
import Kore.Attribute.Attributes (Attributes)
import Kore.Attribute.Axiom (Label (Label), UniqueId (UniqueId), getUniqueId, unLabel)
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.Builtin qualified as Builtin
import Kore.Error (Error (..))
import Kore.Exec qualified as Exec
import Kore.Exec.GraphTraversal qualified as GraphTraversal
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.IndexedModule.MetadataToolsBuilder qualified as MetadataTools
import Kore.Internal.Condition qualified as Condition
import Kore.Internal.OrPattern qualified as OrPattern
import Kore.Internal.Pattern (Pattern)
import Kore.Internal.Pattern qualified as Pattern
import Kore.Internal.Predicate (
    getMultiAndPredicate,
    pattern PredicateTrue,
 )
import Kore.Internal.Predicate qualified as Predicate
import Kore.Internal.Substitution (Assignment, assignedVariable)
import Kore.Internal.Substitution qualified as Substitution
import Kore.Internal.TermLike (TermLike)
import Kore.Internal.TermLike qualified as TermLike
import Kore.JsonRpc.Error
import Kore.JsonRpc.Server (
    ErrorObj (..),
    JsonRpcHandler (JsonRpcHandler),
    Request (getReqId),
    Respond,
    jsonRpcServer,
 )
import Kore.JsonRpc.Types
import Kore.JsonRpc.Types.Log
import Kore.Log.DebugContext qualified as Log
import Kore.Log.DecidePredicateUnknown (
    DecidePredicateUnknown (..),
    externaliseDecidePredicateUnknown,
    srcLoc,
 )
import Kore.Log.InfoExecDepth (ExecDepth (..))
import Kore.Log.InfoJsonRpcProcessRequest (InfoJsonRpcProcessRequest (..))
import Kore.Log.JsonRpc (LogJsonRpcServer (..))
import Kore.Parser (parseKoreModule)
import Kore.Reachability.Claim qualified as Claim
import Kore.Rewrite (
    ProgramState,
    RuleInfo (..),
    extractProgramState,
 )
import Kore.Rewrite.ClaimPattern qualified as ClaimPattern
import Kore.Rewrite.RewriteStep (EnableAssumeInitialDefined (..))
import Kore.Rewrite.RewritingVariable (
    RewritingVariableName (..),
    getRewritingPattern,
    getRewritingVariable,
    isSomeConfigVariable,
    isSomeEquationVariable,
    isSomeRuleVariable,
    mkRewritingPattern,
    mkRewritingTerm,
 )
import Kore.Rewrite.SMT.Evaluator qualified as SMT.Evaluator
import Kore.Rewrite.SMT.Lemma (getSMTLemmas)
import Kore.Rewrite.Timeout (
    EnableMovingAverage (..),
    StepTimeout (..),
 )
import Kore.Simplify.API (evalSimplifier)
import Kore.Simplify.Pattern qualified as Pattern
import Kore.Simplify.Simplify (Simplifier)
import Kore.Syntax (VariableName)
import Kore.Syntax.Definition (Definition (..))
import Kore.Syntax.Json qualified
import Kore.Syntax.Json qualified as PatternJson
import Kore.Syntax.Module (Module (..), ModuleName (..))
import Kore.Syntax.Sentence (
    SentenceAxiom,
 )
import Kore.Syntax.Variable qualified as SomeVariable
import Kore.TopBottom (TopBottom (isTop))
import Kore.Validate.DefinitionVerifier (verifyAndIndexDefinitionWithBase)
import Kore.Validate.PatternVerifier (Context (..))
import Kore.Validate.PatternVerifier qualified as PatternVerifier
import Log qualified
import Network.JSONRPC (fromId)
import Prelude.Kore
import SMT qualified
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)

respond ::
    forall m.
    MonadIO m =>
    String ->
    MVar.MVar ServerState ->
    ModuleName ->
    ( forall a.
      SmtMetadataTools StepperAttributes ->
      [SentenceAxiom (TermLike VariableName)] ->
      SMT.SMT a ->
      IO a
    ) ->
    Respond (API 'Req) m (API 'Res)
respond reqId serverState moduleName runSMT =
    \case
        Execute
            ExecuteRequest
                { state
                , maxDepth
                , _module
                , cutPointRules
                , terminalRules
                , movingAverageStepTimeout
                , assumeStateDefined
                , stepTimeout
                , logSuccessfulRewrites
                , logTiming
                } -> withMainModule (coerce _module) $ \serializedModule lemmas -> do
                start <- liftIO $ getTime Monotonic
                case verifyIn serializedModule state of
                    Left Error{errorError, errorContext} ->
                        pure $
                            Left $
                                backendError $
                                    CouldNotVerifyPattern
                                        [ ErrorWithContext (pack errorError) $
                                            map pack errorContext
                                        ]
                    Right verifiedPattern -> do
                        let tracingEnabled = fromMaybe False logSuccessfulRewrites
                        traversalResult <-
                            liftIO
                                ( runSMT (Exec.metadataTools serializedModule) lemmas $
                                    withContextLog Log.CtxExecute $
                                        Exec.rpcExec
                                            (maybe Unlimited (\(Depth n) -> Limit n) maxDepth)
                                            (coerce stepTimeout)
                                            ( if fromMaybe False movingAverageStepTimeout
                                                then EnableMovingAverage
                                                else DisableMovingAverage
                                            )
                                            ( if fromMaybe False assumeStateDefined
                                                then EnableAssumeInitialDefined
                                                else DisableAssumeInitialDefined
                                            )
                                            tracingEnabled
                                            serializedModule
                                            (toStopLabels cutPointRules terminalRules)
                                            verifiedPattern
                                )

                        stop <- liftIO $ getTime Monotonic
                        let duration =
                                if (fromMaybe False logTiming)
                                    then
                                        Just $
                                            fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 1e9
                                    else Nothing
                        pure $ buildResult duration (TermLike.termLikeSort verifiedPattern) traversalResult
              where
                toStopLabels :: Maybe [Text] -> Maybe [Text] -> Exec.StopLabels
                toStopLabels cpRs tRs =
                    Exec.StopLabels (fromMaybe [] cpRs) (fromMaybe [] tRs)

                containsLabelOrRuleId rules = \case
                    Nothing -> Nothing
                    Just lblsOrRuleIds ->
                        let requestSet =
                                Set.fromList $
                                    concat
                                        [[Left $ Label $ Just lblOrRid, Right $ UniqueId $ Just lblOrRid] | lblOrRid <- lblsOrRuleIds]
                            ruleSet =
                                Set.fromList $
                                    concat [[Left ruleLabel, Right ruleId] | Exec.RuleTrace{ruleId, ruleLabel} <- toList rules]
                         in either unLabel getUniqueId <$> Set.lookupMin (requestSet `Set.intersection` ruleSet)
                mkLogs mbDuration rules
                    | isJust mbDuration
                        || fromMaybe False logSuccessfulRewrites =
                        Just . concat $
                            maybe [] (\t -> [ProcessingTime (Just KoreRpc) t]) mbDuration
                                : [ [ Rewrite
                                        { result =
                                            Success
                                                { rewrittenTerm = Nothing
                                                , substitution = Nothing
                                                , ruleId = fromMaybe "UNKNOWN" $ getUniqueId ruleId
                                                }
                                        , origin = KoreRpc
                                        }
                                    | fromMaybe False logSuccessfulRewrites
                                    ]
                                  | Exec.RuleTrace{ruleId} <- toList rules
                                  ]
                    | otherwise = Nothing

                buildResult ::
                    Maybe Double ->
                    TermLike.Sort ->
                    GraphTraversal.TraversalResult (Exec.RpcExecState RewritingVariableName) ->
                    Either ErrorObj (API 'Res)
                buildResult mbDuration sort = \case
                    GraphTraversal.Ended
                        [Exec.RpcExecState{rpcDepth = ExecDepth depth, rpcProgState = result, rpcRules = rules}] ->
                            -- Actually not "ended" but out of instructions.
                            -- See @toTransitionResult@ in @rpcExec@.
                            Right $
                                Execute $
                                    ExecuteResult
                                        { state = patternToExecState False sort result
                                        , depth = Depth depth
                                        , reason = if Just (Depth depth) == maxDepth then DepthBound else Stuck
                                        , rule = Nothing
                                        , nextStates = Nothing
                                        , logs = mkLogs mbDuration rules
                                        , unknownPredicate = Nothing
                                        }
                    GraphTraversal.GotStuck
                        _n
                        [ GraphTraversal.IsStuck
                                Exec.RpcExecState{rpcDepth = ExecDepth depth, rpcProgState = result, rpcRules = rules}
                            ] ->
                            Right $
                                Execute $
                                    ExecuteResult
                                        { state = patternToExecState False sort result
                                        , depth = Depth depth
                                        , reason = Stuck
                                        , rule = Nothing
                                        , nextStates = Nothing
                                        , logs = mkLogs mbDuration rules
                                        , unknownPredicate = Nothing
                                        }
                    GraphTraversal.GotStuck
                        _n
                        [ GraphTraversal.IsVacuous
                                Exec.RpcExecState{rpcDepth = ExecDepth depth, rpcProgState = result, rpcRules = rules}
                            ] ->
                            Right $
                                Execute $
                                    ExecuteResult
                                        { state = patternToExecState False sort result
                                        , depth = Depth depth
                                        , reason = Vacuous
                                        , rule = Nothing
                                        , nextStates = Nothing
                                        , logs = mkLogs mbDuration rules
                                        , unknownPredicate = Nothing
                                        }
                    GraphTraversal.Stopped
                        [Exec.RpcExecState{rpcDepth = ExecDepth depth, rpcProgState, rpcRules = rules}]
                        nexts
                            | Just rule <- containsLabelOrRuleId (mconcatMap Exec.rpcRules nexts) cutPointRules ->
                                Right $
                                    Execute $
                                        ExecuteResult
                                            { state = patternToExecState False sort rpcProgState
                                            , depth = Depth depth
                                            , reason = CutPointRule
                                            , rule
                                            , nextStates =
                                                Just $ map (patternToExecState False sort . Exec.rpcProgState) nexts
                                            , logs = mkLogs mbDuration rules
                                            , unknownPredicate = Nothing
                                            }
                            | Just rule <- containsLabelOrRuleId rules terminalRules ->
                                Right $
                                    Execute $
                                        ExecuteResult
                                            { state = patternToExecState False sort rpcProgState
                                            , depth = Depth depth
                                            , reason = TerminalRule
                                            , rule
                                            , nextStates = Nothing
                                            , logs = mkLogs mbDuration rules
                                            , unknownPredicate = Nothing
                                            }
                            | otherwise ->
                                Right $
                                    Execute $
                                        ExecuteResult
                                            { state = patternToExecState False sort rpcProgState
                                            , depth = Depth depth
                                            , reason = Branching
                                            , rule = Nothing
                                            , nextStates =
                                                Just $ map (patternToExecState True sort . Exec.rpcProgState) nexts
                                            , logs = mkLogs mbDuration rules
                                            , unknownPredicate = Nothing
                                            }
                    GraphTraversal.TimedOut
                        Exec.RpcExecState{rpcDepth = ExecDepth depth, rpcProgState, rpcRules = rules}
                        _ ->
                            Right $
                                Execute $
                                    ExecuteResult
                                        { state = patternToExecState False sort rpcProgState
                                        , depth = Depth depth
                                        , reason = Timeout
                                        , rule = Nothing
                                        , nextStates = Nothing
                                        , logs = mkLogs mbDuration rules
                                        , unknownPredicate = Nothing
                                        }
                    -- these are programmer errors
                    result@GraphTraversal.Aborted{} ->
                        Left $ backendError $ Kore.JsonRpc.Error.Aborted $ pack $ show result
                    other ->
                        Left $ backendError $ MultipleStates $ pack $ show other

                patternToExecState ::
                    Bool ->
                    TermLike.Sort ->
                    ProgramState (RuleInfo RewritingVariableName) (Pattern RewritingVariableName) ->
                    ExecuteState
                patternToExecState includeRuleInfo sort s
                    | includeRuleInfo =
                        ExecuteState
                            { term
                            , predicate
                            , substitution
                            , ruleSubstitution
                            , rulePredicate
                            , ruleId
                            }
                    | otherwise =
                        ExecuteState
                            { term
                            , predicate
                            , substitution
                            , ruleSubstitution = Nothing
                            , rulePredicate = Nothing
                            , ruleId = Nothing
                            }
                  where
                    term = PatternJson.fromTermLike $ Pattern.term p
                    predicate =
                        case Pattern.predicate p of
                            PredicateTrue -> Nothing
                            pr -> Just $ PatternJson.fromPredicate sort pr
                    substitution =
                        PatternJson.fromSubstitution sort $ Pattern.substitution p
                    (p, rulePredicate, ruleSubstitution, ruleId) = case extractProgramState s of
                        (Nothing, _) -> (Pattern.bottomOf sort, Nothing, Nothing, Nothing)
                        (Just p', Nothing) -> (getRewritingPattern p', Nothing, Nothing, Nothing)
                        (Just p', Just (RuleInfo{rulePredicate = pr, ruleSubstitution = sub, ruleId = UniqueId rid})) ->
                            let subUnwrapped = Substitution.unwrap sub
                                -- any substitutions which are not RuleVariable <var> -> <term> have been added to the substitution list
                                -- via an equation in the requires clause, e.g. X ==Int 0
                                -- hence, we want to copy these into the rule-condition
                                predsFromSub = filter ((isSomeConfigVariable ||| isSomeEquationVariable) . assignedVariable) subUnwrapped
                                pr' = Predicate.fromPredicate sort $ Predicate.mapVariables getRewritingVariable pr
                                finalPr =
                                    if isTop pr
                                        then
                                            if null predsFromSub
                                                then Nothing
                                                else Just $ Kore.Syntax.Json.fromTermLike $ foldl1 TermLike.mkAnd $ map toEquals predsFromSub
                                        else Just $ Kore.Syntax.Json.fromTermLike $ foldl TermLike.mkAnd pr' $ map toEquals predsFromSub
                             in ( getRewritingPattern p'
                                , finalPr
                                , PatternJson.fromSubstitution sort
                                    $ Substitution.mapVariables
                                        ( pure $ \case
                                            ConfigVariableName v -> v
                                            RuleVariableName v@SomeVariable.VariableName{base = TermLike.Id{getId = nm, idLocation = loc}} -> v{SomeVariable.base = TermLike.Id{getId = "Rule" <> nm, idLocation = loc}}
                                            EquationVariableName v@SomeVariable.VariableName{base = TermLike.Id{getId = nm, idLocation = loc}} -> v{SomeVariable.base = TermLike.Id{getId = "Eq" <> nm, idLocation = loc}}
                                        )
                                    $ Substitution.filter isSomeRuleVariable sub
                                , rid
                                )

                    toEquals :: Assignment RewritingVariableName -> TermLike VariableName
                    toEquals (Substitution.Assignment v t) =
                        TermLike.mkEquals sort (TermLike.mkVar $ SomeVariable.mapSomeVariable getRewritingVariable v) $
                            TermLike.mapVariables getRewritingVariable t

                    a ||| b = \v -> a v || b v

        -- Step StepRequest{} -> pure $ Right $ Step $ StepResult []
        Implies ImpliesRequest{antecedent, consequent, _module, logTiming} -> withMainModule (coerce _module) $ \serializedModule lemmas -> do
            start <- liftIO $ getTime Monotonic
            case PatternVerifier.runPatternVerifier (verifierContext serializedModule) verify of
                Left Error{errorError, errorContext} ->
                    pure $
                        Left $
                            backendError $
                                CouldNotVerifyPattern
                                    [ ErrorWithContext (pack errorError) $
                                        map pack errorContext
                                    ]
                Right (antVerified, consVerified) -> do
                    let leftPatt =
                            mkRewritingPattern $ Pattern.parsePatternFromTermLike antVerified
                        sort = TermLike.termLikeSort antVerified
                        (consWOExistentials, existentialVars) =
                            ClaimPattern.termToExistentials $
                                mkRewritingTerm consVerified
                        rightPatt = Pattern.parsePatternFromTermLike consWOExistentials

                    result <-
                        liftIO
                            . runSMT (Exec.metadataTools serializedModule) lemmas
                            . withContextLog Log.CtxImplies
                            . evalInSimplifierContext serializedModule
                            . runExceptT
                            $ Claim.checkSimpleImplication
                                leftPatt
                                rightPatt
                                existentialVars
                    stop <- liftIO $ getTime Monotonic
                    let timeLog =
                            ProcessingTime
                                (Just KoreRpc)
                                (fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 1e9)
                        allLogs =
                            if (fromMaybe False logTiming)
                                then Just [timeLog]
                                else Nothing
                    pure $ buildResult allLogs sort result
          where
            verify = do
                antVerified <-
                    PatternVerifier.verifyStandalonePattern Nothing $
                        PatternJson.toParsedPattern $
                            PatternJson.term antecedent
                consVerified <-
                    PatternVerifier.verifyStandalonePattern Nothing $
                        PatternJson.toParsedPattern $
                            PatternJson.term consequent
                pure (antVerified, consVerified)

            renderCond sort cond =
                let pat = Condition.mapVariables getRewritingVariable cond
                    predicate =
                        PatternJson.fromPredicate sort $ Condition.predicate pat
                    mbSubstitution =
                        PatternJson.fromSubstitution sort $ Condition.substitution pat
                    noSubstitution = PatternJson.fromTermLike $ TermLike.mkTop sort
                 in Condition
                        { predicate
                        , substitution = fromMaybe noSubstitution mbSubstitution
                        }

            buildResult _ _ (Left Error{errorError, errorContext}) =
                Left $
                    backendError $
                        ImplicationCheckError $
                            ErrorWithContext (pack errorError) $
                                map pack errorContext
            buildResult logs sort (Right (term, r)) =
                let jsonTerm =
                        PatternJson.fromTermLike $
                            TermLike.mapVariables getRewritingVariable term
                 in Right . Implies $
                        case r of
                            Claim.Implied Nothing ->
                                ImpliesResult jsonTerm True (Just . renderCond sort $ Condition.bottom) logs
                            Claim.Implied (Just cond) ->
                                ImpliesResult jsonTerm True (Just . renderCond sort $ cond) logs
                            Claim.NotImplied _ ->
                                ImpliesResult jsonTerm False Nothing logs
                            Claim.NotImpliedStuck (Just cond) ->
                                let jsonCond = renderCond sort cond
                                 in ImpliesResult jsonTerm False (Just jsonCond) logs
                            Claim.NotImpliedStuck Nothing ->
                                ImpliesResult jsonTerm False (Just . renderCond sort $ Condition.bottom) logs
        Simplify SimplifyRequest{state, _module, logTiming} -> withMainModule (coerce _module) $ \serializedModule lemmas -> do
            start <- liftIO $ getTime Monotonic
            case verifyIn serializedModule state of
                Left Error{errorError, errorContext} ->
                    pure $
                        Left $
                            backendError $
                                CouldNotVerifyPattern
                                    [ ErrorWithContext (pack errorError) $
                                        map pack errorContext
                                    ]
                Right stateVerified -> do
                    let patt =
                            mkRewritingPattern $ Pattern.parsePatternFromTermLike stateVerified
                        sort = TermLike.termLikeSort stateVerified

                    result <-
                        liftIO
                            . runSMT (Exec.metadataTools serializedModule) lemmas
                            . withContextLog Log.CtxSimplify
                            . evalInSimplifierContext serializedModule
                            $ SMT.Evaluator.filterMultiOr $srcLoc =<< Pattern.simplify patt

                    stop <- liftIO $ getTime Monotonic
                    let timeLog =
                            ProcessingTime
                                (Just KoreRpc)
                                (fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 1e9)
                        allLogs =
                            if (fromMaybe False logTiming)
                                then Just [timeLog]
                                else Nothing
                    pure $
                        Right $
                            Simplify
                                SimplifyResult
                                    { state =
                                        PatternJson.fromTermLike $
                                            TermLike.mapVariables getRewritingVariable $
                                                OrPattern.toTermLike sort result
                                    , logs = allLogs
                                    }
        AddModule AddModuleRequest{_module, nameAsId = nameAsId'} -> runExceptT $ do
            let nameAsId = fromMaybe False nameAsId'
            parsedModule@Module{moduleName = name} <-
                withExceptT (backendError . InvalidModule . ErrorOnly . pack) $
                    liftEither $
                        parseKoreModule "<add-module>" _module
            st@ServerState
                { serializedModules
                , receivedModules
                , loadedDefinition = LoadedDefinition{indexedModules, definedNames, kFileLocations}
                } <-
                liftIO $ MVar.takeMVar serverState
            let moduleHash = ModuleName . fromString . ('m' :) . show . hashWith SHA256 $ encodeUtf8 _module

            -- put the original state back if we fail at any point
            flip catchE (\e -> liftIO (MVar.putMVar serverState st) >> throwError e) $ do
                -- check if we already received a module with this name
                when nameAsId $
                    case Map.lookup (coerce name) receivedModules of
                        -- if a different module was already added, throw error
                        Just m | _module /= m -> throwError $ backendError $ DuplicateModuleName $ coerce name
                        _ -> pure ()

                -- Check for a corner case when we send module M1 with the name "m<hash of M2>"" and name-as-id: true
                -- followed by adding M2. Should not happen in practice...
                case Map.lookup (coerce moduleHash) receivedModules of
                    Just m | _module /= m -> throwError $ backendError $ DuplicateModuleName $ coerce moduleHash
                    _ -> pure ()

                case (Map.lookup (coerce moduleHash) indexedModules, Map.lookup (coerce moduleHash) serializedModules) of
                    (Just foundIdxModule, Just foundSerModule) -> do
                        liftIO $
                            MVar.putMVar serverState $
                                if nameAsId
                                    then -- the module already exists, but re-adding with name because name-as-id is true

                                        ServerState
                                            { serializedModules =
                                                Map.insert (coerce name) foundSerModule serializedModules
                                            , receivedModules = case Map.lookup (coerce moduleHash) receivedModules of
                                                Just recMod -> Map.insert (coerce name) recMod receivedModules
                                                Nothing -> receivedModules
                                            , loadedDefinition =
                                                LoadedDefinition
                                                    { indexedModules = Map.insert (coerce name) foundIdxModule indexedModules
                                                    , definedNames
                                                    , kFileLocations
                                                    }
                                            }
                                    else -- the module already exists so we don't need to add it again
                                        st
                        pure . AddModule $ AddModuleResult (getModuleName moduleHash)
                    _ -> do
                        (newIndexedModules, newDefinedNames) <-
                            withExceptT
                                ( \Error{errorError, errorContext} -> backendError $ InvalidModule $ ErrorWithContext (pack errorError) $ map pack errorContext
                                )
                                $ liftEither
                                $ verifyAndIndexDefinitionWithBase
                                    (indexedModules, definedNames)
                                    Builtin.koreVerifiers
                                    (Definition (def @Attributes) [parsedModule{moduleName = moduleHash}])

                        newModule <-
                            liftEither $
                                maybe (Left $ backendError $ CouldNotFindModule $ coerce moduleHash) Right $
                                    Map.lookup (coerce moduleHash) newIndexedModules

                        let metadataTools = MetadataTools.build newModule
                            lemmas = getSMTLemmas newModule
                        serializedModule <-
                            liftIO
                                . runSMT metadataTools lemmas
                                . withContextLog Log.CtxAddModule
                                $ Exec.makeSerializedModule newModule
                        internedTextCacheHash <- liftIO $ readIORef globalInternedTextCache

                        let serializedDefinition =
                                SerializedDefinition
                                    { serializedModule = serializedModule
                                    , locations = kFileLocations
                                    , internedTextCache = internedTextCacheHash
                                    , lemmas = lemmas
                                    }
                            newSerializedModules =
                                Map.fromList $
                                    if nameAsId
                                        then [(coerce moduleHash, serializedDefinition), (coerce name, serializedDefinition)]
                                        else [(coerce moduleHash, serializedDefinition)]
                            loadedDefinition =
                                LoadedDefinition
                                    { indexedModules = (if nameAsId then Map.insert (coerce name) newModule else id) newIndexedModules
                                    , definedNames = newDefinedNames
                                    , kFileLocations
                                    }

                        liftIO . MVar.putMVar serverState $
                            ServerState
                                { serializedModules =
                                    serializedModules `Map.union` newSerializedModules
                                , loadedDefinition
                                , receivedModules =
                                    (if nameAsId then Map.insert (coerce name) _module else id) $
                                        Map.insert (coerce moduleHash) _module receivedModules
                                }

                        pure . AddModule $ AddModuleResult (getModuleName moduleHash)
        GetModel GetModelRequest{state, _module} ->
            withMainModule (coerce _module) $ \serializedModule lemmas ->
                case verifyIn serializedModule state of
                    Left Error{errorError, errorContext} ->
                        pure $
                            Left $
                                backendError $
                                    CouldNotVerifyPattern
                                        [ ErrorWithContext (pack errorError) $
                                            map pack errorContext
                                        ]
                    Right stateVerified -> do
                        let sort = TermLike.termLikeSort stateVerified
                            patt =
                                mkRewritingPattern $ Pattern.parsePatternFromTermLike stateVerified
                            preds = getMultiAndPredicate $ Condition.predicate patt
                        -- We use the invariant that the parsing does not produce a substitution

                        let tools = Exec.metadataTools serializedModule
                        result <-
                            if all Pattern.isTop preds -- catch terms without predicates
                                then pure $ Left False
                                else
                                    liftIO
                                        . runSMT tools lemmas
                                        . withContextLog Log.CtxGetModel
                                        . SMT.Evaluator.getModelFor tools
                                        $ NonEmpty.fromList preds

                        pure . Right . GetModel $
                            case result of
                                Left False ->
                                    GetModelResult
                                        { satisfiable = Unknown
                                        , substitution = Nothing
                                        }
                                Left True ->
                                    GetModelResult
                                        { satisfiable = Unsat
                                        , substitution = Nothing
                                        }
                                Right subst ->
                                    GetModelResult
                                        { satisfiable = Sat
                                        , substitution =
                                            PatternJson.fromSubstitution sort $
                                                Substitution.mapVariables getRewritingVariable subst
                                        }

        -- this case is only reachable if the cancel appeared as part of a batch request
        Cancel -> pure $ Left cancelUnsupportedInBatchMode
  where
    withContextLog :: Log.SimpleContext -> SMT.SMT a -> SMT.SMT a
    withContextLog method =
        Log.logWhile (Log.DebugContext $ Log.CLWithId $ Log.CtxRequest $ Text.pack reqId)
            . Log.inContext Log.CtxKore
            . Log.inContext method

    withMainModule module' act = do
        let mainModule = fromMaybe moduleName module'
        ServerState{serializedModules} <- liftIO $ MVar.readMVar serverState
        case Map.lookup mainModule serializedModules of
            Nothing -> pure $ Left $ backendError $ CouldNotFindModule $ coerce mainModule
            Just (SerializedDefinition{serializedModule, lemmas}) ->
                act serializedModule lemmas

    verifierContext Exec.SerializedModule{verifiedModule} =
        PatternVerifier.verifiedModuleContext verifiedModule
            & PatternVerifier.withBuiltinVerifiers Builtin.koreVerifiers
            & withRpcRequest
      where
        withRpcRequest context = context{isRpcRequest = True}

    -- verifyIn :: Exec.SerializedModule -> PatternJson.KoreJson -> Either VerifyError (Pattern _)
    verifyIn m state =
        PatternVerifier.runPatternVerifier (verifierContext m) $
            PatternVerifier.verifyStandalonePattern Nothing $
                PatternJson.toParsedPattern (PatternJson.term state)

    evalInSimplifierContext ::
        Exec.SerializedModule -> Simplifier a -> SMT.SMT a
    evalInSimplifierContext
        Exec.SerializedModule
            { sortGraph
            , overloadGraph
            , metadataTools
            , verifiedModule
            , equations
            } =
            evalSimplifier
                verifiedModule
                sortGraph
                overloadGraph
                metadataTools
                equations

data ServerState = ServerState
    { serializedModules :: Map.Map ModuleName SerializedDefinition
    , receivedModules :: Map.Map ModuleName Text
    , loadedDefinition :: LoadedDefinition
    }

runServer ::
    Int ->
    MVar.MVar ServerState ->
    ModuleName ->
    ( forall a.
      SmtMetadataTools StepperAttributes ->
      [SentenceAxiom (TermLike VariableName)] ->
      SMT.SMT a ->
      IO a
    ) ->
    Log.LoggerEnv IO ->
    IO ()
runServer port serverState mainModule runSMT Log.LoggerEnv{logAction} = do
    flip runLoggingT logFun $
        jsonRpcServer
            srvSettings
            ( \req parsed ->
                log (InfoJsonRpcProcessRequest (getReqId req) parsed)
                    >> respond (fromId $ getReqId req) serverState mainModule runSMT parsed
            )
            [ handleDecidePredicateUnknown
            , handleErrorCall
            , handleSomeException
            ]
  where
    srvSettings = serverSettings port "*"

    logFun loc src level msg =
        Log.logWith logAction $ LogJsonRpcServer{loc, src, level, msg}

    log :: MonadIO m => Log.Entry entry => entry -> m ()
    log = Log.logWith $ Log.hoistLogAction liftIO logAction

handleDecidePredicateUnknown :: JsonRpcHandler
handleDecidePredicateUnknown = JsonRpcHandler $ \(err :: DecidePredicateUnknown) ->
    pure
        ( backendError $
            SmtSolverError $
                uncurry ErrorWithTerm $
                    externaliseDecidePredicateUnknown err
        )
