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
import Control.Exception (AsyncException (UserInterrupt), handleJust)
import Control.Monad (forM_, void, when)
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (
    LogLevel (..),
    LoggingT (runLoggingT),
    MonadLoggerIO (askLoggerIO),
    ToLogStr (toLogStr),
 )
import Control.Monad.Logger qualified as Log
import Control.Monad.Logger qualified as Logger
import Data.Aeson.Types (Value (..))
import Data.Conduit.Network (serverSettings)
import Data.IORef (writeIORef)
import Data.InternedText (globalInternedTextCache)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text (decodeUtf8, encodeUtf8)
import Network.JSONRPC
import Options.Applicative
import System.Clock (
    Clock (..),
    TimeSpec (..),
    getTime,
 )
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.Log.FastLogger (newTimeCache)

import Booster.CLOptions
import Booster.Log.Context qualified
import Booster.SMT.Base qualified as SMT (SExpr (..), SMTId (..))
import Booster.SMT.Interface (SMTOptions (..))
import Booster.Trace
import Booster.Util qualified as Booster
import Data.Limit (Limit (..))
import GlobalMain qualified
import Kore.Attribute.Symbol (StepperAttributes)
import Kore.BugReport (BugReportOption (..), withBugReport)
import Kore.IndexedModule.MetadataTools (SmtMetadataTools)
import Kore.Internal.TermLike (TermLike, VariableName)
import Kore.JsonRpc (ServerState (..))
import Kore.JsonRpc qualified as Kore
import Kore.JsonRpc.Error
import Kore.JsonRpc.Server
import Kore.JsonRpc.Types
import Kore.Log.BoosterAdaptor (
    ExeName (..),
    KoreLogType (..),
    LogAction (LogAction),
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
import Kore.Log.DebugSolver qualified as Log
import Kore.Log.Registry qualified as Log
import Kore.Rewrite.SMT.Lemma (declareSMTLemmas)
import Kore.Syntax.Definition (ModuleName (ModuleName), SentenceAxiom)
import Options.SMT as KoreSMT (KoreSolverOptions (..), Solver (..))
import SMT qualified as KoreSMT

data KoreServer = KoreServer
    { serverState :: MVar.MVar Kore.ServerState
    , mainModule :: Text
    , runSMT ::
        forall a.
        SmtMetadataTools StepperAttributes ->
        [SentenceAxiom (TermLike VariableName)] ->
        KoreSMT.SMT a ->
        IO a
    , loggerEnv :: Log.LoggerEnv IO
    }

respond ::
    forall m.
    Log.MonadLogger m =>
    Respond (API 'Req) m (API 'Res) ->
    Respond (API 'Req) m (API 'Res)
respond kore req = case req of
    Execute _ ->
        loggedKore ExecuteM req >>= \case
            Right (Execute koreResult) -> do
                Log.logInfoNS "proxy" . Text.pack $
                    "Kore " <> show koreResult.reason <> " at " <> show koreResult.depth
                pure . Right . Execute $ koreResult
            res -> pure res
    Implies _ -> loggedKore ImpliesM req
    Simplify simplifyReq -> handleSimplify simplifyReq
    AddModule _ -> kore req
    GetModel _ ->
        loggedKore GetModelM req
    Cancel ->
        pure $ Left $ ErrorObj "Cancel not supported" (-32601) Null
  where
    handleSimplify :: SimplifyRequest -> m (Either ErrorObj (API 'Res))
    handleSimplify simplifyReq = do
        let koreReq = Simplify simplifyReq
        koreResult <- kore koreReq
        case koreResult of
            Right (Simplify koreRes) -> do
                pure . Right . Simplify $
                    SimplifyResult
                        { state = koreRes.state
                        , logs = koreRes.logs
                        }
            koreError ->
                pure koreError

    loggedKore method r = do
        Log.logInfoNS "proxy" . Text.pack $ show method <> " (using kore)"
        kore r

main :: IO ()
main = do
    startTime <- getTime Monotonic
    options <- execParser clParser
    let CLProxyOptions
            { clOptions =
                clOPts@CLOptions
                    { port
                    , logLevels
                    , logContexts
                    , logFormat
                    , smtOptions
                    , eventlogEnabledUserEvents
                    , logFile
                    , logTimeStamps
                    }
            } = options
        (logLevel, customLevels) = adjustLogLevels logLevels
        logContextsWithcustomLevelContexts =
            logContexts
                <> concatMap (\case LevelOther o -> fromMaybe [] $ levelToContext Map.!? o; _ -> []) customLevels
        levelFilter :: Logger.LogSource -> LogLevel -> Bool
        levelFilter _source lvl =
            lvl `elem` customLevels || lvl >= logLevel && lvl <= LevelError
        koreLogExtraLevels =
            if not (null logContextsWithcustomLevelContexts)
                then -- context logging: enable all Proxy-required Kore log entries
                    Set.unions . Map.elems $ koreExtraLogs
                else -- no context logging: only enable Kore log entries for the given Proxy log levels
                    Set.unions . mapMaybe (`Map.lookup` koreExtraLogs) $ customLevels
        koreSolverOptions = translateSMTOpts smtOptions

    mTimeCache <- if logTimeStamps then Just <$> (newTimeCache "%Y-%m-%d %T") else pure Nothing

    Booster.withFastLogger mTimeCache logFile $ \stderrLogger mFileLogger ->
        flip runLoggingT (Booster.handleOutput stderrLogger) . Logger.filterLogger levelFilter $ do
            liftIO $ forM_ eventlogEnabledUserEvents $ \t -> do
                putStrLn $ "Tracing " <> show t
                enableCustomUserEvent t

            monadLogger <- askLoggerIO

            let koreLogActions ::
                    forall m.
                    MonadIO m =>
                    [LogAction m Log.SomeEntry]
                koreLogActions = [koreLogAction]
                  where
                    koreLogRenderer = case logFormat of
                        Standard -> renderStandardPretty (ExeName "") (TimeSpec 0 0) TimestampsDisable
                        OneLine -> renderOnelinePretty (ExeName "") (TimeSpec 0 0) TimestampsDisable
                        Json -> renderJson (ExeName "") (TimeSpec 0 0) TimestampsDisable
                    koreLogEarlyFilter = case logFormat of
                        Json -> \l@(Log.SomeEntry ctxt e) ->
                            Log.oneLineJson e /= Null && koreFilterContext (ctxt <> [l])
                        OneLine -> \l@(Log.SomeEntry ctxt _) -> koreFilterContext $ ctxt <> [l]
                        Standard -> const True
                    koreFilterContext ctxt =
                        null logContextsWithcustomLevelContexts
                            || ( let contextStrs =
                                        concatMap
                                            ( \(Log.SomeEntry _ c) -> Text.encodeUtf8 <$> Log.oneLineContextDoc c
                                            )
                                            ctxt
                                  in any (flip Booster.Log.Context.mustMatch contextStrs) logContextsWithcustomLevelContexts
                               )

                    koreLogAction =
                        koreSomeEntryLogAction
                            koreLogRenderer
                            koreLogEarlyFilter
                            (const True)
                            ( LogAction $ \txt -> liftIO $
                                case mFileLogger of
                                    Just fileLogger -> fileLogger $ toLogStr $ txt <> "\n"
                                    Nothing -> stderrLogger $ toLogStr $ txt <> "\n"
                            )

            let coLogLevel = fromMaybe Log.Info $ toSeverity logLevel
                koreLogOptions =
                    (defaultKoreLogOptions (ExeName "") startTime)
                        { Log.logLevel = coLogLevel
                        , Log.logEntries = koreLogExtraLevels
                        , Log.timestampsSwitch = TimestampsDisable
                        , Log.debugSolverOptions =
                            Log.DebugSolverOptions . fmap (<> ".kore") $ smtOptions >>= (.transcript)
                        , Log.logType = LogProxy (mconcat koreLogActions)
                        , Log.logFormat = Log.Standard
                        }
                srvSettings = serverSettings port "*"

            liftIO $ void $ withBugReport (ExeName "kore-rpc-dev") BugReportOnError $ \_reportDirectory ->
                withLogger koreLogOptions $ \actualLogAction -> do
                    mvarLogAction <- newMVar actualLogAction
                    let logAction = swappableLogger mvarLogAction
                    kore@KoreServer{runSMT} <-
                        mkKoreServer Log.LoggerEnv{logAction} clOPts koreSolverOptions
                    runLoggingT (Logger.logInfoNS "proxy" "Starting RPC server") monadLogger

                    let koreRespond :: Respond (API 'Req) (LoggingT IO) (API 'Res)
                        koreRespond = Kore.respond kore.serverState (ModuleName kore.mainModule) runSMT
                        server =
                            jsonRpcServer
                                srvSettings
                                (const $ respond koreRespond)
                                [Kore.handleDecidePredicateUnknown, handleErrorCall, handleSomeException]
                        interruptHandler _ = do
                            when (logLevel >= LevelInfo) $
                                hPutStrLn stderr "[Info#proxy] Server shutting down"
                            exitSuccess
                    handleJust isInterrupt interruptHandler $ runLoggingT server monadLogger
  where
    clParser =
        info
            (clProxyOptionsParser <**> versionInfoParser <**> helper)
            parserInfoModifiers

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

logLevelToKoreLogEntryMap :: Map.Map LogLevel [Text.Text]
logLevelToKoreLogEntryMap =
    Map.fromList
        [ (LevelOther "SimplifyKore", ["DebugAttemptEquation", "DebugApplyEquation"])
        ,
            ( LevelOther "RewriteKore"
            , ["DebugAttemptedRewriteRules", "DebugAppliedLabeledRewriteRule", "DebugAppliedRewriteRules"]
            )
        , (LevelOther "SimplifySuccess", ["DebugApplyEquation"])
        , (LevelOther "RewriteSuccess", ["DebugAppliedRewriteRules"])
        ]

newtype CLProxyOptions = CLProxyOptions
    { clOptions :: CLOptions
    }

parserInfoModifiers :: InfoMod options
parserInfoModifiers =
    fullDesc
        <> header "kore-rpc-dev - a JSON RPC for Kore with the interface of kore-rpc-booster"

clProxyOptionsParser :: Parser CLProxyOptions
clProxyOptionsParser =
    CLProxyOptions
        <$> clOptionsParser

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
    Log.LoggerEnv IO -> CLOptions -> KoreSMT.KoreSolverOptions -> IO KoreServer
mkKoreServer loggerEnv@Log.LoggerEnv{logAction} CLOptions{definitionFile, mainModuleName} koreSolverOptions =
    flip Log.runLoggerT logAction $ do
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
            KoreServer
                { serverState
                , mainModule = mainModuleName
                , runSMT
                , loggerEnv
                }
  where
    KoreSMT.KoreSolverOptions{timeOut, retryLimit, tactic} = koreSolverOptions
    smtConfig :: KoreSMT.Config
    smtConfig =
        KoreSMT.z3Config
            { KoreSMT.executable = KoreSMT.z3Config.executable -- hack to shut up GHC field warning
            , KoreSMT.timeOut = timeOut
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
