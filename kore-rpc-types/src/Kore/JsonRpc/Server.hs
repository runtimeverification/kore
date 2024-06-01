{-# LANGUAGE TypeApplications #-}

{- |
Module      : Kore.Network.JSONRPC
Description : JSON RPC server for kore-rpc
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
module Kore.JsonRpc.Server (
    ErrorObj (..),
    Request (..),
    Respond,
    jsonRpcServer,
    JsonRpcHandler (..),
) where

import Control.Concurrent (forkIO, throwTo)
import Control.Concurrent.STM.TMChan (closeTMChan, newTMChan, readTMChan, writeTMChan)
import Control.Exception (Exception (fromException), catch, mask, throw)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Logger qualified as Log
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Data.Aeson (ToJSON, Value (Null))
import Data.Aeson.Encode.Pretty as Json
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Conduit (ConduitT, Void, runConduit, (.|))
import Data.Conduit.List qualified as CL
import Data.Conduit.Network (ServerSettings, appSink, appSource, runGeneralTCPServer)
import Data.Conduit.TMChan (closeTBMChan, sinkTBMChan, sourceTBMChan)
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Kore.JsonRpc.Types (FromRequestCancellable (isCancel), ReqException (..), rpcJsonConfig)
import Network.JSONRPC hiding (encodeConduit, runJSONRPCT)
import UnliftIO (MonadUnliftIO, atomically, wait, withAsync)

-- Conduit to encode JSON to ByteString.
encodeConduit :: (ToJSON j, Monad m) => Json.Config -> ConduitT j ByteString m ()
encodeConduit encodeOpts = CL.mapM $ \m -> return . L8.toStrict $ encodePretty' encodeOpts m

{- | Create JSON-RPC session around conduits from transport layer.  When
 context exits session disappears.
-}
runJSONRPCT ::
    (MonadLoggerIO m, MonadUnliftIO m) =>
    -- | aeson-pretty Config
    Json.Config ->
    -- | JSON-RPC version
    Ver ->
    -- | Ignore incoming requests/notifs
    Bool ->
    -- | Sink to send messages
    ConduitT ByteString Void m () ->
    -- | Source to receive messages from
    ConduitT () ByteString m () ->
    -- | JSON-RPC action
    JSONRPCT m a ->
    -- | Output of action
    m a
runJSONRPCT encodeOpts ver ignore snk src f = do
    qs <- liftIO . atomically $ initSession ver ignore
    let inSnk = sinkTBMChan (inCh qs)
        outSrc = sourceTBMChan (outCh qs)
        decodeInput = do
            runConduit $ src .| decodeConduit ver .| inSnk
            liftIO (atomically $ closeTBMChan $ inCh qs)
        encodeOutput = runConduit $ outSrc .| encodeConduit encodeOpts .| snk
    withAsync decodeInput $
        const $
            withAsync encodeOutput $ \o ->
                withAsync (runReaderT processIncoming qs) $
                    const $ do
                        a <- runReaderT f qs
                        liftIO $ do
                            atomically . closeTBMChan $ outCh qs
                            _ <- wait o
                            return a

-- | TCP server transport for JSON-RPC.
jsonRpcServer ::
    (MonadLoggerIO m, MonadUnliftIO m, FromRequestCancellable q, ToJSON r) =>
    -- | Connection settings
    ServerSettings ->
    -- | Action to perform on connecting client thread
    (Request -> Respond q (Log.LoggingT IO) r) ->
    [JsonRpcHandler] ->
    m a
jsonRpcServer serverSettings respond handlers =
    runGeneralTCPServer serverSettings $ \cl ->
        runJSONRPCT
            -- we have to ensure that the returned messages contain no newlines
            -- inside the message and have a trailing newline, which is used as the delimiter
            rpcJsonConfig{Json.confIndent = Spaces 0, Json.confTrailingNewline = True}
            V2
            False
            (appSink cl)
            (appSource cl)
            (srv respond handlers)

data JsonRpcHandler = forall e. Exception e => JsonRpcHandler (e -> Log.LoggingT IO ErrorObj)

srv ::
    forall m q r.
    (MonadLoggerIO m, FromRequestCancellable q, ToJSON r) =>
    (Request -> Respond q (Log.LoggingT IO) r) ->
    [JsonRpcHandler] ->
    JSONRPCT m ()
srv respond handlers = do
    reqQueue <- liftIO $ atomically newTMChan
    let mainLoop tid =
            let loop =
                    receiveBatchRequest >>= \case
                        Nothing ->
                            atomically $ closeTMChan reqQueue
                        Just (SingleRequest req) | Right True <- isCancel @q <$> fromRequest req -> do
                            Log.logInfoN "Cancel request"
                            liftIO $ throwTo tid CancelRequest
                            loop
                        Just req -> do
                            forM_ (getRequests req) $ \r -> do
                                Log.logInfoN $ "Process request " <> mReqId r <> " " <> getReqMethod r
                                Log.logDebugN $ Text.pack $ show r
                            liftIO $ atomically $ writeTMChan reqQueue req
                            loop
             in loop
    spawnWorker reqQueue >>= mainLoop
    Log.logInfoN "Session terminated"
  where
    isRequest = \case
        Request{} -> True
        _ -> False

    getRequests = \case
        SingleRequest r -> [r]
        BatchRequest rs -> rs

    mReqId = \case
        Request _ _ _ (IdTxt i) -> i
        Request _ _ _ (IdInt i) -> Text.pack $ show i
        Notif{} -> ""

    cancelError = ErrorObj "Request cancelled" (-32000) Null

    spawnWorker reqQueue = do
        rpcSession <- ask
        logger <- Log.askLoggerIO
        let withLog :: Log.LoggingT IO a -> IO a
            withLog = flip Log.runLoggingT logger

            sendResponses :: BatchResponse -> Log.LoggingT IO ()
            sendResponses r = flip runReaderT rpcSession $ sendBatchResponse r

            respondTo :: Request -> Log.LoggingT IO (Maybe Response)
            respondTo req = buildResponse (respond req) req

            cancelReq :: ErrorObj -> Maybe BatchRequest -> Log.LoggingT IO Bool
            cancelReq err = \case
                Just (SingleRequest req@Request{}) -> do
                    let reqVersion = getReqVer req
                        reqId = getReqId req
                    sendResponses $ SingleResponse $ ResponseError reqVersion err reqId
                    pure True
                Just (SingleRequest Notif{}) -> pure True
                Just (BatchRequest reqs) -> do
                    sendResponses $
                        BatchResponse $
                            [ResponseError (getReqVer req) err (getReqId req) | req <- reqs, isRequest req]
                    pure True
                Nothing -> pure False

            processReq :: BatchRequest -> Log.LoggingT IO ()
            processReq = \case
                SingleRequest req -> do
                    rM <- respondTo req
                    mapM_ (sendResponses . SingleResponse) rM
                BatchRequest reqs -> do
                    rs <- catMaybes <$> mapM respondTo reqs
                    sendResponses $ BatchResponse rs

            catchesHandler a e = foldr tryHandler (throw e) $ JsonRpcHandler (\(_ :: ReqException) -> pure cancelError) : handlers
              where
                tryHandler (JsonRpcHandler handler) res =
                    case fromException e of
                        Just e' ->
                            withLog $ do
                                err <- handler e'
                                cancelReq err a
                        Nothing -> res

            bracketOnReqException before thing =
                mask $ \restore -> do
                    a <- before
                    restore (thing a) `catch` catchesHandler a

            loop = do
                shouldContinue <-
                    bracketOnReqException
                        (atomically $ readTMChan reqQueue)
                        ( \case
                            Nothing -> pure False
                            Just req -> withLog (processReq req) >> pure True
                        )
                when shouldContinue loop
        liftIO $ forkIO loop
