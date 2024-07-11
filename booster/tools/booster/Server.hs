{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

{- |
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Main (main) where

import Control.Concurrent.MVar (newMVar)
import Control.Concurrent.MVar qualified as MVar
import Control.DeepSeq (force)
import Control.Exception (AsyncException (UserInterrupt), evaluate, handleJust)
import Control.Monad (forM_, unless, void)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (
    LogLevel (..),
    runNoLoggingT,
 )
import Control.Monad.Trans.Reader (runReaderT)
import Data.Aeson qualified as JSON
import Data.Bifunctor (bimap)
import Data.ByteString.Char8 qualified as BS
import Data.Coerce (coerce)
import Data.Conduit.Network (serverSettings)
import Data.IORef (writeIORef)
import Data.InternedText (globalInternedTextCache)
import Data.Limit (Limit (..))
import Data.List (intercalate)
import Data.List.Extra (splitOn)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (decodeUtf8, encodeUtf8)
import Network.JSONRPC qualified as JSONRPC
import Options.Applicative
import System.Clock (
    Clock (..),
    getTime,
 )
import System.Environment qualified as Env
import System.Exit
import System.IO (hPutStrLn, stderr)

import Booster.CLOptions
import Booster.Definition.Attributes.Base (
    ComputedAxiomAttributes (notPreservesDefinednessReasons),
 )
import Booster.Definition.Base (
    KoreDefinition (..),
    RewriteRule (computedAttributes),
 )
import Booster.Definition.Ceil (ComputeCeilSummary (..), computeCeilsDefinition)
import Booster.GlobalState
import Booster.JsonRpc qualified as Booster
import Booster.LLVM.Internal (mkAPI, withDLib)
import Booster.Log hiding (withLogger)
import Booster.Log.Context qualified as Ctxt
import Booster.Pattern.Base (Predicate (..))
import Booster.Pattern.Pretty
import Booster.Prettyprinter (renderOneLineText)
import Booster.SMT.Base qualified as SMT (SExpr (..), SMTId (..))
import Booster.SMT.Interface (SMTOptions (..))
import Booster.Syntax.Json.Externalise (externaliseTerm)
import Booster.Syntax.ParsedKore (loadDefinition)
import Booster.Trace
import Booster.Util qualified as Booster
import Data.Data (Proxy)
import GlobalMain qualified
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.BugReport (BugReportOption (..), withBugReport)
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.TermLike (TermLike, VariableName)
import Kore.JsonRpc (ServerState (..))
import Kore.JsonRpc qualified as Kore
import Kore.JsonRpc.Error hiding (Aborted, error)
import Kore.JsonRpc.Server
import Kore.JsonRpc.Types (API, HaltReason (..), ReqOrRes (Req, Res))
import Kore.JsonRpc.Types.Depth (Depth (..))
import Kore.Log.BoosterAdaptor (
    ExeName (..),
    KoreLogType (..),
    TimestampsSwitch (TimestampsDisable),
    defaultKoreLogOptions,
    koreSomeEntryLogAction,
    renderJson,
    renderOnelinePretty,
    renderStandardPretty,
    swappableLogger,
    withLogger,
 )
import Kore.Log.BoosterAdaptor qualified as Log
import Kore.Log.DebugContext qualified as Log
import Kore.Log.DebugSolver qualified as Log
import Kore.Log.Registry qualified as Log
import Kore.Rewrite.SMT.Lemma (declareSMTLemmas)
import Kore.Syntax.Definition (ModuleName (ModuleName), SentenceAxiom)
import Network.JSONRPC (fromId)
import Options.SMT as KoreSMT (KoreSolverOptions (..), Solver (..))
import Pretty qualified
import Proxy (KoreServer (..), ProxyConfig (..))
import Proxy qualified
import SMT qualified as KoreSMT
import Stats qualified

envName :: String
envName = "KORE_RPC_OPTS" -- aligned with legacy kore-rpc

main :: IO ()
main = do
    startTime <- getTime Monotonic
    options <- do
        Env.lookupEnv envName >>= \case
            Nothing -> execParser clParser
            Just envArgs -> do
                hPutStrLn stderr $ "[proxy] Reading additional server options from " <> envName
                args <- Env.getArgs
                Env.withArgs (words envArgs <> args) $ execParser clParser
    let CLProxyOptions
            { clOptions =
                clOPts@CLOptions
                    { definitionFile
                    , mainModuleName
                    , port
                    , llvmLibraryFile
                    , logLevels
                    , logFormat
                    , logTimeStamps
                    , timeStampsFormat
                    , logContexts
                    , logFile
                    , smtOptions
                    , equationOptions
                    , indexCells
                    , prettyPrintOptions
                    , eventlogEnabledUserEvents
                    }
            , proxyOptions =
                ProxyOptions
                    { forceFallback
                    , boosterSMT
                    , fallbackReasons
                    , simplifyAtEnd
                    , simplifyBeforeFallback
                    }
            } = options
        (logLevel, customLevels) = adjustLogLevels logLevels
        logContextsWithcustomLevelContexts =
            logContexts
                <> concatMap (\case LevelOther o -> fromMaybe [] $ levelToContext Map.!? o; _ -> []) customLevels
        contextLoggingEnabled = not (null logContextsWithcustomLevelContexts)
        koreSolverOptions = translateSMTOpts smtOptions
        timestampFlag = case timeStampsFormat of
            Pretty -> Booster.PrettyTimestamps
            Nanoseconds -> Booster.NoPrettyTimestamps

    mTimeCache <-
        if logTimeStamps then Just <$> Booster.newTimeCache timestampFlag else pure Nothing

    Booster.withFastLogger mTimeCache logFile $ \stderrLogger mFileLogger -> do
        liftIO $ forM_ eventlogEnabledUserEvents $ \t -> do
            putStrLn $ "Tracing " <> show t
            enableCustomUserEvent t

        let koreLogRenderer = case logFormat of
                Standard -> renderStandardPretty
                OneLine -> renderOnelinePretty
                Json -> renderJson
            koreLogFilter = case logFormat of
                Standard -> const True
                _ -> \l@(Log.SomeEntry ctxt _) -> not (Log.isEmpty l) && koreFilterContext (ctxt <> [l])
            koreFilterContext ctxt =
                not contextLoggingEnabled
                    || ( let contextStrs =
                                concatMap
                                    ( \(Log.SomeEntry _ c) -> BS.pack . show <$> Log.oneLineContextDoc c
                                    )
                                    ctxt
                          in any (flip Ctxt.mustMatch contextStrs) logContextsWithcustomLevelContexts
                       )

            koreLogEntries =
                if contextLoggingEnabled
                    then -- context logging: enable all Proxy-required Kore log entries
                        Set.unions . Map.elems $ koreExtraLogs
                    else -- no context logging: only enable Kore log entries for the given Proxy log levels
                        Set.unions . mapMaybe (`Map.lookup` koreExtraLogs) $ customLevels

            boosterContextLogger = case logFormat of
                Json -> Booster.Log.jsonLogger $ fromMaybe stderrLogger mFileLogger
                _ -> Booster.Log.textLogger $ fromMaybe stderrLogger mFileLogger
            filteredBoosterContextLogger =
                flip Booster.Log.filterLogger boosterContextLogger $ \(Booster.Log.LogMessage (Booster.Flag alwaysDisplay) ctxts _) ->
                    alwaysDisplay
                        || let ctxt = map (Text.encodeUtf8 . Booster.Log.toTextualLog) ctxts
                            in any (flip Ctxt.mustMatch ctxt) logContextsWithcustomLevelContexts

            runBoosterLogger :: Booster.Log.LoggerT IO a -> IO a
            runBoosterLogger =
                flip runReaderT (filteredBoosterContextLogger, toModifiersRep prettyPrintOptions)
                    . Booster.Log.unLoggerT

            koreLogActions :: forall m. MonadIO m => [Log.LogAction m Log.SomeEntry]
            koreLogActions = [koreLogAction]
              where
                koreLogAction =
                    koreSomeEntryLogAction
                        koreLogRenderer
                        koreLogFilter
                        (fromMaybe stderrLogger mFileLogger)

        runBoosterLogger $
            Booster.Log.withContext CtxProxy $
                Booster.Log.logMessage' $
                    Text.pack $
                        "Loading definition from "
                            <> definitionFile
                            <> ", main module "
                            <> show mainModuleName

        liftIO $ void $ withBugReport (ExeName "kore-rpc-booster") BugReportOnError $ \_reportDirectory -> withMDLib llvmLibraryFile $ \mdl -> do
            let coLogLevel = fromMaybe Log.Info $ toSeverity logLevel
                koreLogOptions =
                    (defaultKoreLogOptions (ExeName "") startTime)
                        { Log.logLevel = coLogLevel
                        , Log.logEntries = koreLogEntries
                        , Log.timestampsSwitch = TimestampsDisable
                        , Log.debugSolverOptions =
                            Log.DebugSolverOptions . fmap (<> ".kore") $ smtOptions >>= (.transcript)
                        , Log.logType = LogProxy (mconcat koreLogActions)
                        , Log.logFormat = Log.Standard
                        }
                srvSettings = serverSettings port "*"
            withLogger koreLogOptions $ \actualLogAction -> do
                mLlvmLibrary <- maybe (pure Nothing) (fmap Just . mkAPI) mdl
                definitionsWithCeilSummaries <-
                    liftIO $
                        loadDefinition indexCells definitionFile
                            >>= mapM (mapM (runNoLoggingT . computeCeilsDefinition mLlvmLibrary))
                            >>= evaluate . force . either (error . show) id
                unless (isJust $ Map.lookup mainModuleName definitionsWithCeilSummaries) $ do
                    runBoosterLogger $
                        Booster.Log.withContext CtxProxy $
                            Booster.Log.logMessage' $
                                "Main module " <> mainModuleName <> " not found in " <> Text.pack definitionFile
                    liftIO exitFailure

                liftIO $
                    runBoosterLogger $
                        getPrettyModifiers >>= \case
                            ModifiersRep (_ :: FromModifiersT mods => Proxy mods) -> Booster.Log.withContext CtxInfo $ -- FIXME "ceil" $
                                forM_ (Map.elems definitionsWithCeilSummaries) $ \(KoreDefinition{simplifications}, summaries) -> do
                                    forM_ summaries $ \ComputeCeilSummary{rule, ceils} ->
                                        Booster.Log.withRuleContext rule $ do
                                            Booster.Log.withContext CtxInfo -- FIXME "partial-symbols"
                                                $ Booster.Log.logMessage
                                                $ Booster.Log.WithJsonMessage
                                                    (JSON.toJSON rule.computedAttributes.notPreservesDefinednessReasons)
                                                $ renderOneLineText
                                                $ Pretty.hsep
                                                $ Pretty.punctuate Pretty.comma
                                                $ map
                                                    (pretty' @mods)
                                                    rule.computedAttributes.notPreservesDefinednessReasons
                                            unless (null ceils)
                                                $ Booster.Log.withContext CtxInfo -- FIXME"computed-ceils"
                                                $ Booster.Log.logMessage
                                                $ Booster.Log.WithJsonMessage
                                                    ( JSON.object
                                                        ["ceils" JSON..= (bimap (externaliseTerm . coerce) externaliseTerm <$> Set.toList ceils)]
                                                    )
                                                $ renderOneLineText
                                                $ Pretty.hsep
                                                $ Pretty.punctuate Pretty.comma
                                                $ map
                                                    (either (pretty' @mods) (\t -> "#Ceil(" Pretty.<+> pretty' @mods t Pretty.<+> ")"))
                                                    (Set.toList ceils)

                                    forM_ (concat $ concatMap Map.elems simplifications) $ \s ->
                                        unless (null s.computedAttributes.notPreservesDefinednessReasons)
                                            $ Booster.Log.withRuleContext s
                                            $ Booster.Log.withContext CtxInfo -- FIXME"partial-symbols"
                                            $ Booster.Log.logMessage
                                            $ Booster.Log.WithJsonMessage
                                                (JSON.toJSON s.computedAttributes.notPreservesDefinednessReasons)
                                            $ renderOneLineText
                                            $ Pretty.hsep
                                            $ Pretty.punctuate Pretty.comma
                                            $ map
                                                (pretty' @mods)
                                                s.computedAttributes.notPreservesDefinednessReasons
                mvarLogAction <- newMVar actualLogAction
                let logAction = swappableLogger mvarLogAction

                kore@KoreServer{runSMT} <-
                    mkKoreServer Log.LoggerEnv{logAction} clOPts koreSolverOptions

                boosterState <-
                    liftIO $
                        newMVar
                            Booster.ServerState
                                { definitions = Map.map fst definitionsWithCeilSummaries
                                , defaultMain = mainModuleName
                                , mLlvmLibrary
                                , mSMTOptions = if boosterSMT then smtOptions else Nothing
                                , addedModules = mempty
                                }
                statsVar <- Stats.newStats

                writeGlobalEquationOptions equationOptions

                runBoosterLogger $
                    Booster.Log.withContext CtxProxy $
                        Booster.Log.logMessage' ("Starting RPC server" :: Text)

                let koreRespond, boosterRespond :: JSONRPC.Id -> Respond (API 'Req) (Booster.Log.LoggerT IO) (API 'Res)
                    koreRespond reqId = Kore.respond (fromId reqId) kore.serverState (ModuleName kore.mainModule) runSMT
                    boosterRespond reqId =
                        Booster.Log.withContextFor reqId
                            . Booster.Log.withContext CtxBooster
                            . Booster.respond boosterState

                    proxyConfig =
                        ProxyConfig
                            { statsVar
                            , forceFallback
                            , boosterState
                            , fallbackReasons
                            , simplifyAtEnd
                            , simplifyBeforeFallback
                            , customLogLevels = customLevels
                            }
                    server =
                        jsonRpcServer
                            srvSettings
                            ( \rawReq req ->
                                let reqId = getReqId rawReq
                                 in runBoosterLogger $
                                        logRequestId reqId
                                            >> Proxy.respondEither proxyConfig (boosterRespond reqId) (koreRespond reqId) req
                            )
                            [ Kore.handleDecidePredicateUnknown
                            , Booster.handleSmtError
                            , handleErrorCall
                            , handleSomeException
                            ]
                    interruptHandler _ =
                        runBoosterLogger . Booster.Log.withContext CtxProxy $ do
                            Booster.Log.logMessage' @_ @Text "Server shutting down"
                            ( liftIO (Stats.finaliseStats statsVar)
                                    >>= Booster.Log.withContext CtxTiming . Booster.Log.logMessage
                                )
                            liftIO exitSuccess
                handleJust isInterrupt interruptHandler $ runBoosterLogger server
  where
    clParser =
        info
            (clProxyOptionsParser <**> versionInfoParser <**> helper)
            parserInfoModifiers

    withMDLib Nothing f = f Nothing
    withMDLib (Just fp) f = withDLib fp $ \dl -> f (Just dl)

    logRequestId rid =
        Booster.Log.withContext CtxProxy $
            Booster.Log.logMessage' $
                Text.pack $
                    "Processing request " <> fromId rid

    isInterrupt :: AsyncException -> Maybe ()
    isInterrupt UserInterrupt = Just ()
    isInterrupt _other = Nothing

toSeverity :: LogLevel -> Maybe Log.Severity
toSeverity LevelDebug = Just Log.Debug
toSeverity LevelInfo = Just Log.Info
toSeverity LevelWarn = Just Log.Warning
toSeverity LevelError = Just Log.Error
toSeverity LevelOther{} = Nothing

koreExtraLogs :: Map.Map LogLevel Log.EntryTypes
koreExtraLogs =
    Map.map
        (Set.fromList . mapMaybe (`Map.lookup` Log.textToType Log.registry))
        logLevelToKoreLogEntryMap

logLevelToKoreLogEntryMap :: Map.Map LogLevel [Text]
logLevelToKoreLogEntryMap =
    Map.fromList
        [ (LevelOther "SimplifyKore", ["DebugAttemptEquation", "DebugTerm"])
        ,
            ( LevelOther "RewriteKore"
            ,
                [ "DebugAttemptedRewriteRules"
                , "DebugAppliedLabeledRewriteRule"
                , "DebugAppliedRewriteRules"
                , "DebugRewriteRulesRemainder"
                , "DebugTerm"
                ]
            )
        , (LevelOther "SimplifySuccess", ["DebugTerm"])
        , (LevelOther "RewriteSuccess", ["DebugAppliedRewriteRules", "DebugTerm"])
        ]

data CLProxyOptions = CLProxyOptions
    { clOptions :: CLOptions
    , proxyOptions :: ProxyOptions
    }

data ProxyOptions = ProxyOptions
    { forceFallback :: Maybe Depth
    -- ^ force fallback every n-steps
    , boosterSMT :: Bool
    -- ^ whether to use an SMT solver in booster code (but keeping kore-rpc's SMT solver)
    , fallbackReasons :: [HaltReason]
    -- ^ halt reasons to re-execute (fallback) to double-check the result
    , simplifyAtEnd :: Bool
    -- ^ whether to run a post-exec simplification
    , simplifyBeforeFallback :: Bool
    -- ^ whether to run a simplification before fall-back execute requests
    }

parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> header "Haskell Backend Booster Proxy - a JSON RPC server combining kore and booster backends"

clProxyOptionsParser :: Parser CLProxyOptions
clProxyOptionsParser =
    CLProxyOptions
        <$> clOptionsParser
        <*> parseProxyOptions
  where
    parseProxyOptions =
        ProxyOptions
            <$> optional
                ( option
                    (Depth <$> auto)
                    ( metavar "INTERIM_SIMPLIFICATION"
                        <> long "interim-simplification"
                        <> help "Perform pattern-wide simplification every N steps"
                        <> showDefault
                    )
                )
            <*> flag
                True
                False
                ( long "no-booster-smt"
                    <> help "Disable SMT solver for booster code (but keep enabled for legacy code)"
                )
            <*> option
                (eitherReader $ mapM reasonReader . splitOn ",")
                ( long "fallback-on"
                    <> metavar "REASON1,REASON2..."
                    <> value [Branching, Stuck, Aborted]
                    <> help "Halt reasons for which requests should be re-executed with kore-rpc"
                    <> showDefaultWith (intercalate "," . map show)
                )
            <*> flag
                True
                False
                ( long "no-post-exec-simplify"
                    <> help "disable post-exec simplification"
                )
            <*> flag
                True
                False
                ( long "no-fallback-simplify"
                    <> help "disable simplification before fallback requests"
                )

    reasonReader :: String -> Either String HaltReason
    reasonReader = \case
        "Branching" -> Right Branching
        "Stuck" -> Right Stuck
        "Aborted" -> Right Aborted
        other -> Left $ "Reason `" <> other <> "' not supported"

translateSMTOpts :: Maybe SMTOptions -> KoreSMT.KoreSolverOptions
translateSMTOpts = \case
    Just smtOpts ->
        defaultKoreSolverOptions
            { timeOut = KoreSMT.TimeOut . Limit . fromIntegral $ smtOpts.timeout
            , retryLimit =
                KoreSMT.RetryLimit . maybe Unlimited (Limit . fromIntegral) $ smtOpts.retryLimit
            , tactic = fmap translateSExpr smtOpts.tactic
            }
    Nothing ->
        defaultKoreSolverOptions{solver = KoreSMT.None}
  where
    defaultKoreSolverOptions =
        KoreSMT.KoreSolverOptions
            { timeOut = KoreSMT.TimeOut Unlimited
            , retryLimit = KoreSMT.RetryLimit Unlimited
            , rLimit = KoreSMT.RLimit Unlimited
            , resetInterval = KoreSMT.ResetInterval 100
            , prelude = KoreSMT.Prelude Nothing
            , solver = KoreSMT.Z3
            , tactic = Nothing
            }
    translateSExpr :: SMT.SExpr -> KoreSMT.SExpr
    translateSExpr (SMT.Atom (SMT.SMTId x)) = KoreSMT.Atom (Text.decodeUtf8 x)
    translateSExpr (SMT.List ss) = KoreSMT.List $ map translateSExpr ss

mkKoreServer ::
    Log.LoggerEnv IO -> CLOptions -> KoreSolverOptions -> IO KoreServer
mkKoreServer loggerEnv@Log.LoggerEnv{logAction} CLOptions{definitionFile, mainModuleName} koreSolverOptions =
    flip Log.runLoggerT logAction $ Log.logWhile (Log.DebugContext $ Log.CLNullary CtxKore) $ do
        sd@GlobalMain.SerializedDefinition{internedTextCache} <-
            GlobalMain.deserializeDefinition
                koreSolverOptions
                definitionFile
                (ModuleName mainModuleName)
        liftIO $ writeIORef globalInternedTextCache internedTextCache

        loadedDefinition <- GlobalMain.loadDefinitions [definitionFile]
        serverState <-
            liftIO $
                MVar.newMVar
                    Kore.ServerState
                        { serializedModules = Map.singleton (ModuleName mainModuleName) sd
                        , receivedModules = mempty
                        , loadedDefinition
                        }

        pure $
            Proxy.KoreServer
                { serverState
                , mainModule = mainModuleName
                , runSMT
                , loggerEnv
                }
  where
    KoreSMT.KoreSolverOptions{timeOut, retryLimit, tactic} = koreSolverOptions
    smtConfig :: KoreSMT.Config
    smtConfig =
        KoreSMT.defaultConfig
            { KoreSMT.executable = KoreSMT.defaultConfig.executable
            , -- hack to shut up GHC field warning
              KoreSMT.timeOut = timeOut
            , KoreSMT.retryLimit = retryLimit
            , KoreSMT.tactic = tactic
            }

    -- SMT solver with user declared lemmas
    runSMT ::
        forall a.
        SmtMetadataTools StepperAttributes ->
        [SentenceAxiom (TermLike VariableName)] ->
        KoreSMT.SMT a ->
        IO a
    runSMT metadataTools lemmas m =
        flip Log.runLoggerT logAction $
            bracket (KoreSMT.newSolver smtConfig) KoreSMT.stopSolver $ \refSolverHandle -> do
                let userInit = KoreSMT.runWithSolver $ declareSMTLemmas metadataTools lemmas
                    solverSetup =
                        KoreSMT.SolverSetup
                            { userInit
                            , refSolverHandle
                            , config = smtConfig
                            }
                KoreSMT.initSolver solverSetup
                KoreSMT.runWithSolver m solverSetup
