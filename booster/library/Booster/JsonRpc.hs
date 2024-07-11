{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

{- |
Module      : Booster.JsonRpc
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Booster.JsonRpc (
    ServerState (..),
    respond,
    handleSmtError,
    RpcTypes.rpcJsonConfig,
    execStateToKoreJson,
    toExecState,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, putMVar, readMVar, takeMVar)
import Control.Exception qualified as Exception
import Control.Monad
import Control.Monad.Extra (whenJust)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (catchE, except, runExcept, runExceptT, throwE, withExceptT)
import Crypto.Hash (SHA256 (..), hashWith)
import Data.Bifunctor (second)
import Data.Foldable
import Data.List (singleton)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Proxy qualified
import Data.Sequence (Seq)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Records
import Numeric.Natural
import Prettyprinter (comma, hsep, punctuate, (<+>))
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)

import Booster.Definition.Attributes.Base (UniqueId, getUniqueId, uniqueId)
import Booster.Definition.Base (KoreDefinition (..))
import Booster.Definition.Base qualified as Definition (RewriteRule (..))
import Booster.LLVM as LLVM (API)
import Booster.Log
import Booster.Pattern.ApplyEquations qualified as ApplyEquations
import Booster.Pattern.Base (Pattern (..), Sort (SortApp), Term, Variable)
import Booster.Pattern.Base qualified as Pattern
import Booster.Pattern.Bool (pattern TrueBool)
import Booster.Pattern.Match (FailReason (..), MatchResult (..), MatchType (..), matchTerms)
import Booster.Pattern.Pretty
import Booster.Pattern.Rewrite (
    RewriteFailed (..),
    RewriteResult (..),
    RewriteTrace (..),
    performRewrite,
 )
import Booster.Pattern.Util (
    sortOfPattern,
    substituteInPredicate,
    substituteInTerm,
 )
import Booster.Prettyprinter (renderDefault, renderText)
import Booster.SMT.Base qualified as SMT
import Booster.SMT.Interface qualified as SMT
import Booster.Syntax.Json (KoreJson (..), addHeader, prettyPattern, sortOfJson)
import Booster.Syntax.Json.Externalise
import Booster.Syntax.Json.Internalise (
    InternalisedPredicates (..),
    TermOrPredicates (..),
    internalisePattern,
    internaliseTermOrPredicate,
    logPatternError,
    patternErrorToRpcError,
    pattern CheckSubsorts,
    pattern DisallowAlias,
 )
import Booster.Syntax.ParsedKore (parseKoreModule)
import Booster.Syntax.ParsedKore.Base hiding (ParsedModule)
import Booster.Syntax.ParsedKore.Base qualified as ParsedModule (ParsedModule (..))
import Booster.Syntax.ParsedKore.Internalise (
    addToDefinitions,
    definitionErrorToRpcError,
    extractExistentials,
 )
import Booster.Util (Flag (..), constructorName)
import Kore.JsonRpc.Error qualified as RpcError
import Kore.JsonRpc.Server (ErrorObj (..), JsonRpcHandler (..), Respond)
import Kore.JsonRpc.Types qualified as RpcTypes
import Kore.JsonRpc.Types.Log
import Kore.Syntax.Json.Types (Id (..))
import Kore.Syntax.Json.Types qualified as KoreJson
import Kore.Syntax.Json.Types qualified as Syntax

respond ::
    forall m.
    LoggerMIO m =>
    MVar ServerState ->
    Respond (RpcTypes.API 'RpcTypes.Req) m (RpcTypes.API 'RpcTypes.Res)
respond stateVar request =
    getPrettyModifiers >>= \case
        ModifiersRep (_ :: FromModifiersT mods => Data.Proxy.Proxy mods) -> case request of
            RpcTypes.Execute req
                | isJust req.stepTimeout -> pure $ Left $ RpcError.unsupportedOption ("step-timeout" :: String)
                | isJust req.movingAverageStepTimeout ->
                    pure $ Left $ RpcError.unsupportedOption ("moving-average-step-timeout" :: String)
            RpcTypes.Execute req -> withModule req._module $ \(def, mLlvmLibrary, mSMTOptions) -> Booster.Log.withContext CtxExecute $ do
                start <- liftIO $ getTime Monotonic
                -- internalise given constrained term
                let internalised = runExcept $ internalisePattern DisallowAlias CheckSubsorts Nothing def req.state.term

                case internalised of
                    Left patternError -> do
                        void $ Booster.Log.withContext CtxInternalise $ logPatternError patternError
                        pure $
                            Left $
                                RpcError.backendError $
                                    RpcError.CouldNotVerifyPattern
                                        [ patternErrorToRpcError patternError
                                        ]
                    Right (pat, substitution, unsupported) -> do
                        unless (null unsupported) $ do
                            withKorePatternContext (KoreJson.KJAnd (externaliseSort $ sortOfPattern pat) unsupported) $
                                logMessage ("ignoring unsupported predicate parts" :: Text)
                        let cutPoints = fromMaybe [] req.cutPointRules
                            terminals = fromMaybe [] req.terminalRules
                            mbDepth = fmap RpcTypes.getNat req.maxDepth
                            doTracing =
                                Flag $
                                    any
                                        (fromMaybe False)
                                        [ req.logSuccessfulRewrites
                                        , req.logFailedRewrites
                                        , req.logFallbacks
                                        ]
                        -- apply the given substitution before doing anything else
                        let substPat =
                                Pattern
                                    { term = substituteInTerm substitution pat.term
                                    , constraints = Set.map (substituteInPredicate substitution) pat.constraints
                                    , ceilConditions = pat.ceilConditions
                                    }

                        solver <- traverse (SMT.initSolver def) mSMTOptions
                        result <-
                            performRewrite doTracing def mLlvmLibrary solver mbDepth cutPoints terminals substPat
                        whenJust solver SMT.finaliseSolver
                        stop <- liftIO $ getTime Monotonic
                        let duration =
                                if fromMaybe False req.logTiming
                                    then
                                        Just $
                                            fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 1e9
                                    else Nothing
                        pure $ execResponse duration req result substitution unsupported
            RpcTypes.AddModule RpcTypes.AddModuleRequest{_module, nameAsId = nameAsId'} -> Booster.Log.withContext CtxAddModule $ runExceptT $ do
                -- block other request executions while modifying the server state
                state <- liftIO $ takeMVar stateVar
                let nameAsId = fromMaybe False nameAsId'
                    moduleHash = Text.pack $ ('m' :) . show . hashWith SHA256 $ Text.encodeUtf8 _module
                    restoreStateAndRethrow err = do
                        liftIO (putMVar stateVar state)
                        throwE $ RpcError.backendError err
                    listNames :: (HasField "name" a b, HasField "getId" b Text) => [a] -> Text
                    listNames = Text.intercalate ", " . map (.name.getId)

                flip catchE restoreStateAndRethrow $ do
                    newModule <-
                        withExceptT (RpcError.InvalidModule . RpcError.ErrorOnly . pack) $
                            except $
                                parseKoreModule "rpc-request" _module

                    unless (null newModule.sorts) $
                        throwE $
                            RpcError.InvalidModule . RpcError.ErrorOnly $
                                "Module introduces new sorts: " <> listNames newModule.sorts

                    unless (null newModule.symbols) $
                        throwE $
                            RpcError.InvalidModule . RpcError.ErrorOnly $
                                "Module introduces new symbols: " <> listNames newModule.symbols

                    -- check if we already received a module with this name
                    when nameAsId $
                        case Map.lookup (getId newModule.name) state.addedModules of
                            -- if a different module was already added, throw error
                            Just m | _module /= m -> throwE $ RpcError.DuplicateModuleName $ getId newModule.name
                            _ -> pure ()

                    -- Check for a corner case when we send module M1 with the name "m<hash of M2>"" and name-as-id: true
                    -- followed by adding M2. Should not happen in practice...
                    case Map.lookup moduleHash state.addedModules of
                        Just m | _module /= m -> throwE $ RpcError.DuplicateModuleName moduleHash
                        _ -> pure ()

                    newDefinitions <-
                        withExceptT (RpcError.InvalidModule . definitionErrorToRpcError) $
                            except $
                                runExcept $
                                    addToDefinitions newModule{ParsedModule.name = Id moduleHash} state.definitions

                    liftIO $
                        putMVar
                            stateVar
                            state
                                { definitions =
                                    if nameAsId
                                        then Map.insert (getId newModule.name) (newDefinitions Map.! moduleHash) newDefinitions
                                        else newDefinitions
                                , addedModules =
                                    (if nameAsId then Map.insert (getId newModule.name) _module else id) $
                                        Map.insert moduleHash _module state.addedModules
                                }
                    Booster.Log.logMessage $
                        "Added a new module. Now in scope: " <> Text.intercalate ", " (Map.keys newDefinitions)
                    pure $ RpcTypes.AddModule $ RpcTypes.AddModuleResult moduleHash
            RpcTypes.Simplify req -> withModule req._module $ \(def, mLlvmLibrary, mSMTOptions) -> Booster.Log.withContext CtxSimplify $ do
                start <- liftIO $ getTime Monotonic
                let internalised =
                        runExcept $ internaliseTermOrPredicate DisallowAlias CheckSubsorts Nothing def req.state.term
                let mkTraces duration
                        | Just True <- req.logTiming =
                            Just [ProcessingTime (Just Booster) duration]
                        | otherwise =
                            Nothing

                solver <- traverse (SMT.initSolver def) mSMTOptions

                result <- case internalised of
                    Left patternErrors -> do
                        forM_ patternErrors $ \patternError ->
                            void $ Booster.Log.withContext CtxInternalise $ logPatternError patternError
                        pure $
                            Left $
                                RpcError.backendError $
                                    RpcError.CouldNotVerifyPattern $
                                        map patternErrorToRpcError patternErrors
                    -- term and predicate (pattern)
                    Right (TermAndPredicates pat substitution unsupported) -> do
                        unless (null unsupported) $ do
                            withKorePatternContext (KoreJson.KJAnd (externaliseSort $ sortOfPattern pat) unsupported) $ do
                                logMessage ("ignoring unsupported predicate parts" :: Text)
                        -- apply the given substitution before doing anything else
                        let substPat =
                                Pattern
                                    { term = substituteInTerm substitution pat.term
                                    , constraints = Set.map (substituteInPredicate substitution) pat.constraints
                                    , ceilConditions = pat.ceilConditions
                                    }
                        ApplyEquations.evaluatePattern def mLlvmLibrary solver mempty substPat >>= \case
                            (Right newPattern, _) -> do
                                let (term, mbPredicate, mbSubstitution) = externalisePattern newPattern substitution
                                    tSort = externaliseSort (sortOfPattern newPattern)
                                    result = case catMaybes (mbPredicate : mbSubstitution : map Just unsupported) of
                                        [] -> term
                                        ps -> KoreJson.KJAnd tSort $ term : ps
                                pure $ Right (addHeader result)
                            (Left ApplyEquations.SideConditionFalse{}, _) -> do
                                let tSort = externaliseSort $ sortOfPattern pat
                                pure $ Right (addHeader $ KoreJson.KJBottom tSort)
                            (Left (ApplyEquations.EquationLoop _terms), _) ->
                                pure . Left . RpcError.backendError $ RpcError.Aborted "equation loop detected"
                            (Left other, _) ->
                                pure . Left . RpcError.backendError $ RpcError.Aborted (Text.pack . constructorName $ other)
                    -- predicate only
                    Right (Predicates ps)
                        | null ps.boolPredicates && null ps.ceilPredicates && null ps.substitution && null ps.unsupported ->
                            pure $
                                Right
                                    (addHeader $ Syntax.KJTop (fromMaybe (error "not a predicate") $ sortOfJson req.state.term))
                        | otherwise -> do
                            unless (null ps.unsupported) $ do
                                withKorePatternContext (KoreJson.KJAnd (externaliseSort $ SortApp "SortBool" []) ps.unsupported) $ do
                                    logMessage ("ignoring unsupported predicate parts" :: Text)
                            -- apply the given substitution before doing anything else
                            let predicates = map (substituteInPredicate ps.substitution) $ Set.toList ps.boolPredicates
                            withContext CtxConstraint $
                                ApplyEquations.simplifyConstraints
                                    def
                                    mLlvmLibrary
                                    solver
                                    mempty
                                    predicates
                                    >>= \case
                                        (Right newPreds, _) -> do
                                            let predicateSort =
                                                    fromMaybe (error "not a predicate") $
                                                        sortOfJson req.state.term
                                                result =
                                                    map (externalisePredicate predicateSort) newPreds
                                                        <> map (externaliseCeil predicateSort) (Set.toList ps.ceilPredicates)
                                                        <> map (uncurry $ externaliseSubstitution predicateSort) (Map.toList ps.substitution)
                                                        <> ps.unsupported

                                            pure $ Right (addHeader $ Syntax.KJAnd predicateSort result)
                                        (Left something, _) ->
                                            pure . Left . RpcError.backendError $ RpcError.Aborted $ renderText $ pretty' @mods something
                whenJust solver SMT.finaliseSolver
                stop <- liftIO $ getTime Monotonic

                let duration =
                        fromIntegral (toNanoSecs (diffTimeSpec stop start)) / 1e9
                    mkSimplifyResponse state =
                        RpcTypes.Simplify
                            RpcTypes.SimplifyResult{state, logs = mkTraces duration}
                pure $ second mkSimplifyResponse result
            RpcTypes.GetModel req -> withModule req._module $ \case
                (_, _, Nothing) -> do
                    withContext CtxGetModel $
                        logMessage' ("get-model request, not supported without SMT solver" :: Text)
                    pure $ Left RpcError.notImplemented
                (def, _, Just smtOptions) -> do
                    let internalised =
                            runExcept $
                                internaliseTermOrPredicate DisallowAlias CheckSubsorts Nothing def req.state.term
                    case internalised of
                        Left patternErrors -> do
                            forM_ patternErrors $ \patternError ->
                                void $ Booster.Log.withContext CtxInternalise $ logPatternError patternError
                            pure $
                                Left $
                                    RpcError.backendError $
                                        RpcError.CouldNotVerifyPattern $
                                            map patternErrorToRpcError patternErrors
                        -- various predicates obtained
                        Right things -> do
                            -- term and predicates were sent. Only work on predicates
                            (boolPs, suppliedSubst) <-
                                case things of
                                    TermAndPredicates pat substitution unsupported -> do
                                        withContext CtxGetModel $
                                            logMessage' ("ignoring supplied terms and only checking predicates" :: Text)

                                        unless (null unsupported) $ do
                                            withContext CtxGetModel $ do
                                                logMessage' ("ignoring unsupported predicates" :: Text)
                                                withContext CtxDetail $
                                                    logMessage (Text.unwords $ map prettyPattern unsupported)
                                        pure (Set.toList pat.constraints, substitution)
                                    Predicates ps -> do
                                        unless (null ps.ceilPredicates && null ps.unsupported) $ do
                                            withContext CtxGetModel $ do
                                                logMessage' ("ignoring supplied ceils and unsupported predicates" :: Text)
                                                withContext CtxDetail $
                                                    logMessage
                                                        ( Text.unlines $
                                                            map
                                                                (renderText . ("#Ceil:" <>) . pretty' @mods)
                                                                (Set.toList ps.ceilPredicates)
                                                                <> map prettyPattern ps.unsupported
                                                        )
                                        pure (Set.toList ps.boolPredicates, ps.substitution)

                            smtResult <-
                                if null boolPs && Map.null suppliedSubst
                                    then do
                                        -- as per spec, no predicate, no answer
                                        withContext CtxGetModel $
                                            withContext CtxSMT $
                                                logMessage ("No predicates or substitutions given, returning Unknown" :: Text)
                                        pure $ Left SMT.Unknown
                                    else do
                                        solver <- SMT.initSolver def smtOptions
                                        result <- SMT.getModelFor solver boolPs suppliedSubst
                                        SMT.finaliseSolver solver
                                        case result of
                                            Left err -> liftIO $ Exception.throw err -- fail hard on SMT errors
                                            Right response -> pure response
                            withContext CtxGetModel $
                                withContext CtxSMT $
                                    logMessage $
                                        "SMT result: " <> pack (either show (("Subst: " <>) . show . Map.size) smtResult)
                            pure . Right . RpcTypes.GetModel $ case smtResult of
                                Left SMT.Unsat ->
                                    RpcTypes.GetModelResult
                                        { satisfiable = RpcTypes.Unsat
                                        , substitution = Nothing
                                        }
                                Left SMT.ReasonUnknown{} ->
                                    RpcTypes.GetModelResult
                                        { satisfiable = RpcTypes.Unknown
                                        , substitution = Nothing
                                        }
                                Left SMT.Unknown ->
                                    RpcTypes.GetModelResult
                                        { satisfiable = RpcTypes.Unknown
                                        , substitution = Nothing
                                        }
                                Left other ->
                                    error $ "Unexpected result " <> show other <> " from getModelFor"
                                Right subst ->
                                    let sort = fromMaybe (error "Unknown sort in input") $ sortOfJson req.state.term
                                        substitution
                                            | Map.null subst = Nothing
                                            | [(var, term)] <- Map.assocs subst =
                                                Just . addHeader $
                                                    KoreJson.KJEquals
                                                        (externaliseSort var.variableSort)
                                                        sort
                                                        (externaliseTerm $ Pattern.Var var)
                                                        (externaliseTerm term)
                                            | otherwise =
                                                Just . addHeader $
                                                    KoreJson.KJAnd
                                                        sort
                                                        [ KoreJson.KJEquals
                                                            (externaliseSort var.variableSort)
                                                            sort
                                                            (externaliseTerm $ Pattern.Var var)
                                                            (externaliseTerm term)
                                                        | (var, term) <- Map.assocs subst
                                                        ]
                                     in RpcTypes.GetModelResult
                                            { satisfiable = RpcTypes.Sat
                                            , substitution
                                            }
            RpcTypes.Implies req -> withModule req._module $ \(def, mLlvmLibrary, mSMTOptions) -> Booster.Log.withContext CtxImplies $ do
                -- internalise given constrained term
                let internalised =
                        runExcept . internalisePattern DisallowAlias CheckSubsorts Nothing def . fst . extractExistentials

                case (internalised req.antecedent.term, internalised req.consequent.term) of
                    (Left patternError, _) -> do
                        void $ Booster.Log.withContext CtxInternalise $ logPatternError patternError
                        pure $
                            Left $
                                RpcError.backendError $
                                    RpcError.CouldNotVerifyPattern
                                        [ patternErrorToRpcError patternError
                                        ]
                    (_, Left patternError) -> do
                        void $ Booster.Log.withContext CtxInternalise $ logPatternError patternError
                        pure $
                            Left $
                                RpcError.backendError $
                                    RpcError.CouldNotVerifyPattern
                                        [ patternErrorToRpcError patternError
                                        ]
                    (Right (patL, substitutionL, unsupportedL), Right (patR, substitutionR, unsupportedR)) -> do
                        unless (null unsupportedL && null unsupportedR) $ do
                            logMessage'
                                ("aborting due to unsupported predicate parts" :: Text)
                            unless (null unsupportedL) $
                                withContext CtxDetail $
                                    logMessage
                                        (Text.unwords $ map prettyPattern unsupportedL)
                            unless (null unsupportedR) $
                                withContext CtxDetail $
                                    logMessage
                                        (Text.unwords $ map prettyPattern unsupportedR)
                        let
                            -- apply the given substitution before doing anything else
                            substPatL =
                                Pattern
                                    { term = substituteInTerm substitutionL patL.term
                                    , constraints = Set.map (substituteInPredicate substitutionL) patL.constraints
                                    , ceilConditions = patL.ceilConditions
                                    }
                            substPatR =
                                Pattern
                                    { term = substituteInTerm substitutionR patR.term
                                    , constraints = Set.map (substituteInPredicate substitutionR) patR.constraints
                                    , ceilConditions = patR.ceilConditions
                                    }

                        case matchTerms Booster.Pattern.Match.Implies def substPatR.term substPatL.term of
                            MatchFailed (SubsortingError sortError) ->
                                pure . Left . RpcError.backendError . RpcError.ImplicationCheckError . RpcError.ErrorOnly . pack $
                                    show sortError
                            MatchFailed{} ->
                                doesNotImply (sortOfPattern substPatL) req.antecedent.term req.consequent.term
                            MatchIndeterminate remainder ->
                                pure . Left . RpcError.backendError . RpcError.ImplicationCheckError . RpcError.ErrorOnly . pack $
                                    "match remainder: "
                                        <> renderDefault
                                            ( hsep $
                                                punctuate comma $
                                                    map (\(t1, t2) -> pretty' @mods t1 <+> "==" <+> pretty' @mods t2) $
                                                        NonEmpty.toList remainder
                                            )
                            MatchSuccess subst -> do
                                let filteredConsequentPreds =
                                        Set.map (substituteInPredicate subst) substPatR.constraints `Set.difference` substPatL.constraints
                                solver <- traverse (SMT.initSolver def) mSMTOptions

                                if null filteredConsequentPreds
                                    then implies (sortOfPattern substPatL) req.antecedent.term req.consequent.term subst
                                    else
                                        ApplyEquations.evaluateConstraints def mLlvmLibrary solver mempty filteredConsequentPreds >>= \case
                                            (Right newPreds, _) ->
                                                if all (== Pattern.Predicate TrueBool) newPreds
                                                    then implies (sortOfPattern substPatL) req.antecedent.term req.consequent.term subst
                                                    else pure . Left . RpcError.backendError $ RpcError.Aborted "unknown constrains"
                                            (Left other, _) ->
                                                pure . Left . RpcError.backendError $ RpcError.Aborted (Text.pack . constructorName $ other)

            -- this case is only reachable if the cancel appeared as part of a batch request
            RpcTypes.Cancel -> pure $ Left RpcError.cancelUnsupportedInBatchMode
  where
    withModule ::
        Maybe Text ->
        ( (KoreDefinition, Maybe LLVM.API, Maybe SMT.SMTOptions) ->
          m (Either ErrorObj (RpcTypes.API 'RpcTypes.Res))
        ) ->
        m (Either ErrorObj (RpcTypes.API 'RpcTypes.Res))
    withModule mbMainModule action = do
        state <- liftIO $ readMVar stateVar
        let mainName = fromMaybe state.defaultMain mbMainModule
        case Map.lookup mainName state.definitions of
            Nothing -> pure $ Left $ RpcError.backendError $ RpcError.CouldNotFindModule mainName
            Just d -> action (d, state.mLlvmLibrary, state.mSMTOptions)

    doesNotImply s l r =
        pure $
            Right $
                RpcTypes.Implies
                    RpcTypes.ImpliesResult
                        { implication = addHeader $ Syntax.KJImplies (externaliseSort s) l r
                        , valid = False
                        , condition = Nothing
                        , logs = Nothing
                        }

    implies s' l r subst =
        let s = externaliseSort s'
         in pure $
                Right $
                    RpcTypes.Implies
                        RpcTypes.ImpliesResult
                            { implication = addHeader $ Syntax.KJImplies s l r
                            , valid = True
                            , condition =
                                Just
                                    RpcTypes.Condition
                                        { predicate = addHeader $ Syntax.KJTop s
                                        , substitution =
                                            addHeader
                                                $ (\xs -> if null xs then Syntax.KJTop s else Syntax.KJAnd s xs)
                                                    . map (uncurry $ externaliseSubstitution s)
                                                    . Map.toList
                                                $ subst
                                        }
                            , logs = Nothing
                            }

handleSmtError :: JsonRpcHandler
handleSmtError = JsonRpcHandler $ \case
    SMT.GeneralSMTError err -> runtimeError "problem" err
    SMT.SMTTranslationError err -> runtimeError "translation" err
    SMT.SMTSolverUnknown reason premises preds -> do
        let bool = externaliseSort Pattern.SortBool -- predicates are terms of sort Bool
            externalise = Syntax.KJAnd bool . map (externalisePredicate bool) . Set.toList
            allPreds = addHeader $ Syntax.KJAnd bool [externalise premises, externalise preds]
        pure $ RpcError.backendError $ RpcError.SmtSolverError $ RpcError.ErrorWithTerm reason allPreds
  where
    runtimeError prefix err = do
        let msg = "SMT " <> prefix <> ": " <> err
        pure $ RpcError.runtimeError msg

data ServerState = ServerState
    { definitions :: Map Text KoreDefinition
    -- ^ definitions for each loaded module as main module
    , defaultMain :: Text
    -- ^ default main module (initially from command line, could be changed later)
    , mLlvmLibrary :: Maybe LLVM.API
    -- ^ optional LLVM simplification library
    , mSMTOptions :: Maybe SMT.SMTOptions
    -- ^ (optional) SMT solver options
    , addedModules :: Map Text Text
    -- ^ map of raw modules added via add-module
    }

execStateToKoreJson :: RpcTypes.ExecuteState -> KoreJson.KoreJson
execStateToKoreJson RpcTypes.ExecuteState{term = t, substitution, predicate} =
    let subAndPred = catMaybes [KoreJson.term <$> substitution, KoreJson.term <$> predicate]
        innerSorts = mapMaybe sortOfJson $ KoreJson.term t : subAndPred
        topLevelSort = case innerSorts of
            [] -> KoreJson.SortApp (KoreJson.Id "SortGeneratedTopCell") []
            xs ->
                if all (== head xs) (tail xs) -- we know xs is non-empty, hence `head` is safe here
                    then KoreJson.SortApp (head xs).name []
                    else KoreJson.SortApp (KoreJson.Id "SortGeneratedTopCell") []
     in t
            { KoreJson.term =
                if null subAndPred then t.term else KoreJson.KJAnd topLevelSort (t.term : subAndPred)
            }

execResponse ::
    Maybe Double ->
    RpcTypes.ExecuteRequest ->
    (Natural, Seq (RewriteTrace ()), RewriteResult Pattern) ->
    Map Variable Term ->
    [Syntax.KorePattern] ->
    Either ErrorObj (RpcTypes.API 'RpcTypes.Res)
execResponse mbDuration req (d, traces, rr) originalSubstitution unsupported = case rr of
    RewriteBranch p nexts ->
        Right $
            RpcTypes.Execute
                RpcTypes.ExecuteResult
                    { reason = RpcTypes.Branching
                    , depth
                    , logs
                    , state = toExecState p originalSubstitution unsupported Nothing
                    , nextStates =
                        Just $
                            map (\(_, muid, p') -> toExecState p' originalSubstitution unsupported (Just muid)) $
                                toList nexts
                    , rule = Nothing
                    , unknownPredicate = Nothing
                    }
    RewriteStuck p ->
        Right $
            RpcTypes.Execute
                RpcTypes.ExecuteResult
                    { reason = RpcTypes.Stuck
                    , depth
                    , logs
                    , state = toExecState p originalSubstitution unsupported Nothing
                    , nextStates = Nothing
                    , rule = Nothing
                    , unknownPredicate = Nothing
                    }
    RewriteTrivial p ->
        Right $
            RpcTypes.Execute
                RpcTypes.ExecuteResult
                    { reason = RpcTypes.Vacuous
                    , depth
                    , logs
                    , state = toExecState p originalSubstitution unsupported Nothing
                    , nextStates = Nothing
                    , rule = Nothing
                    , unknownPredicate = Nothing
                    }
    RewriteCutPoint lbl _ p next ->
        Right $
            RpcTypes.Execute
                RpcTypes.ExecuteResult
                    { reason = RpcTypes.CutPointRule
                    , depth
                    , logs
                    , state = toExecState p originalSubstitution unsupported Nothing
                    , nextStates = Just [toExecState next originalSubstitution unsupported Nothing]
                    , rule = Just lbl
                    , unknownPredicate = Nothing
                    }
    RewriteTerminal lbl _ p ->
        Right $
            RpcTypes.Execute
                RpcTypes.ExecuteResult
                    { reason = RpcTypes.TerminalRule
                    , depth
                    , logs
                    , state = toExecState p originalSubstitution unsupported Nothing
                    , nextStates = Nothing
                    , rule = Just lbl
                    , unknownPredicate = Nothing
                    }
    RewriteFinished _ _ p ->
        Right $
            RpcTypes.Execute
                RpcTypes.ExecuteResult
                    { reason = RpcTypes.DepthBound
                    , depth
                    , logs
                    , state = toExecState p originalSubstitution unsupported Nothing
                    , nextStates = Nothing
                    , rule = Nothing
                    , unknownPredicate = Nothing
                    }
    RewriteAborted failure p -> do
        Right $
            RpcTypes.Execute
                RpcTypes.ExecuteResult
                    { reason = RpcTypes.Aborted
                    , depth
                    , logs =
                        let abortRewriteLog =
                                mkLogRewriteTrace
                                    (logSuccessfulRewrites, logFailedRewrites)
                                    (RewriteStepFailed failure)
                         in logs <|> abortRewriteLog
                    , state = toExecState p originalSubstitution unsupported Nothing
                    , nextStates = Nothing
                    , rule = Nothing
                    , unknownPredicate = Nothing
                    }
  where
    logSuccessfulRewrites = fromMaybe False req.logSuccessfulRewrites
    logFailedRewrites = fromMaybe False req.logFailedRewrites
    depth = RpcTypes.Depth d

    logs =
        let traceLogs =
                concat . catMaybes . toList $
                    fmap
                        ( mkLogRewriteTrace
                            (logSuccessfulRewrites, logFailedRewrites)
                        )
                        traces
            timingLog =
                fmap (ProcessingTime $ Just Booster) mbDuration
         in case (timingLog, traceLogs) of
                (Nothing, []) -> Nothing
                (Nothing, xs@(_ : _)) -> Just xs
                (Just t, xs) -> Just (t : xs)

toExecState ::
    Pattern -> Map Variable Term -> [Syntax.KorePattern] -> Maybe UniqueId -> RpcTypes.ExecuteState
toExecState pat sub unsupported muid =
    RpcTypes.ExecuteState
        { term = addHeader t
        , predicate = addHeader <$> addUnsupported p
        , substitution = addHeader <$> s
        , ruleSubstitution = Nothing
        , rulePredicate = Nothing
        , ruleId = getUniqueId <$> muid
        }
  where
    (t, p, s) = externalisePattern pat sub
    termSort = externaliseSort $ sortOfPattern pat
    allUnsupported = Syntax.KJAnd termSort unsupported
    addUnsupported
        | null unsupported = id
        | otherwise = maybe (Just allUnsupported) (Just . Syntax.KJAnd termSort . (: unsupported))

mkLogRewriteTrace ::
    (Bool, Bool) ->
    RewriteTrace () ->
    Maybe [LogEntry]
mkLogRewriteTrace
    (logSuccessfulRewrites, logFailedRewrites) =
        \case
            RewriteSingleStep _ uid _ _res
                | logSuccessfulRewrites ->
                    Just $
                        singleton $
                            Kore.JsonRpc.Types.Log.Rewrite
                                { result =
                                    Success
                                        { rewrittenTerm = Nothing
                                        , substitution = Nothing
                                        , ruleId = getUniqueId uid
                                        }
                                , origin = Booster
                                }
            RewriteBranchingStep _ _ -> Nothing -- we may or may not want to emit a trace here in the future
            RewriteStepFailed reason
                | logFailedRewrites ->
                    Just $
                        singleton $
                            Kore.JsonRpc.Types.Log.Rewrite
                                { result = case reason of
                                    NoApplicableRules{} -> Failure{reason = "No applicable rules found", _ruleId = Nothing}
                                    TermIndexIsNone{} -> Failure{reason = "Term index is None for term", _ruleId = Nothing}
                                    RuleApplicationUnclear r _ _ ->
                                        Failure
                                            { reason = "Uncertain about unification of rule"
                                            , _ruleId = Just $ getUniqueId (uniqueId $ Definition.attributes r)
                                            }
                                    RuleConditionUnclear r _ ->
                                        Failure
                                            { reason = "Uncertain about a condition in rule"
                                            , _ruleId = Just $ getUniqueId (uniqueId $ Definition.attributes r)
                                            }
                                    DefinednessUnclear r _ undefReasons ->
                                        Failure
                                            { reason = "Uncertain about definedness of rule because of: " <> pack (show undefReasons)
                                            , _ruleId = Just $ getUniqueId (uniqueId $ Definition.attributes r)
                                            }
                                    RewriteSortError r _ _ ->
                                        Failure
                                            { reason = "Sort error while unifying"
                                            , _ruleId = Just $ getUniqueId (uniqueId $ Definition.attributes r)
                                            }
                                    InternalMatchError{} ->
                                        Failure
                                            { reason = "Internal match error"
                                            , _ruleId = Nothing
                                            }
                                , origin = Booster
                                }
            RewriteSimplified{} -> Just []
            _ -> Nothing
