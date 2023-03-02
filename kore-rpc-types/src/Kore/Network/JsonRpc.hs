{- |
Module      : Kore.Network.JSONRPC
Description : JSON RPC server for kore-rpc
Copyright   : (c) Runtime Verification, 2022
License     : BSD-3-Clause
-}
{-# LANGUAGE TypeApplications #-}
module Kore.Network.JsonRpc (jsonRpcServer, JsonRpcHandler(..)) where

import Control.Concurrent.STM.TChan (newTChan, readTChan, writeTChan)
import Control.Exception (ErrorCall (..), SomeException, mask, Exception (fromException), throw, catch)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Reader (runReaderT, MonadReader (ask), ReaderT)
import Data.Aeson (ToJSON, Value(Null))
import Data.Aeson.Encode.Pretty as Json
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Conduit (ConduitT, Void, runConduit, (.|))
import Data.Conduit.List qualified as CL
import Data.Conduit.Network (ServerSettings, appSink, appSource, runGeneralTCPServer)
import Data.Conduit.TMChan (closeTBMChan, sinkTBMChan, sourceTBMChan)
import Network.JSONRPC hiding (runJSONRPCT, encodeConduit)
import UnliftIO (MonadUnliftIO, atomically, wait, withAsync)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Control.Monad.Logger as Log
import qualified Data.Text as Text
import Data.Maybe (catMaybes)
import Control.Concurrent (forkIO, throwTo)
import Control.Monad (forever)
import Kore.JsonRpc.Types (ReqException(..), FromRequestCancellable (isCancel))

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
    withAsync (runConduit $ src .| decodeConduit ver .| inSnk) $
        const $
            withAsync (runConduit $ outSrc .| encodeConduit encodeOpts .| snk) $ \o ->
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
    -- | aeson-pretty Config
    Json.Config ->
    -- | JSON-RPC version
    Ver ->
    -- | Ignore incoming requests or notifications
    Bool ->
    -- | Connection settings
    ServerSettings ->
    -- | Action to perform on connecting client thread
    (Request -> Respond q (Log.LoggingT IO) r) ->
    [JsonRpcHandler ()] ->
    m a
jsonRpcServer encodeOpts ver ignore serverSettings respond handlers =
    runGeneralTCPServer serverSettings $ \cl ->
        runJSONRPCT
            -- we have to ensure that the returned messages contain no newlines
            -- inside the message and have a trailing newline, which is used as the delimiter
            encodeOpts{Json.confIndent = Spaces 0, Json.confTrailingNewline = True}
            ver
            ignore
            (appSink cl)
            (appSource cl)
            (srv respond handlers)


data JsonRpcHandler a = forall e . Exception e => JsonRpcHandler (e -> Log.LoggingT IO ErrorObj)


srv :: forall m q r. (MonadLoggerIO m, FromRequestCancellable q, ToJSON r) => (Request -> Respond q (Log.LoggingT IO) r) -> [JsonRpcHandler ()] -> JSONRPCT m ()
srv respond handlers = do
    reqQueue <- liftIO $ atomically newTChan
    let mainLoop tid =
            let loop =
                    receiveBatchRequest >>= \case
                        Nothing -> do
                            return ()
                        Just (SingleRequest req) | Right True <- isCancel @q <$> fromRequest req -> do
                            Log.logInfoN "Cancel Request"
                            liftIO $ throwTo tid CancelRequest
                            loop
                        Just req -> do
                            Log.logInfoN $ Text.pack (show req)
                            liftIO $ atomically $ writeTChan reqQueue req
                            loop
            in loop
    spawnWorker reqQueue >>= mainLoop
  where
    isRequest = \case
        Request{} -> True
        _ -> False

    cancelError = ErrorObj "Request cancelled" (-32000) Null

    spawnWorker reqQueue = do
        rpcSession <- ask
        logger <- Log.askLoggerIO
        let withLog :: Log.LoggingT IO a -> IO a
            withLog = flip Log.runLoggingT logger

            sendResponses :: BatchResponse -> Log.LoggingT IO ()
            sendResponses r = flip Log.runLoggingT logger $ flip runReaderT rpcSession $ sendBatchResponse r
            respondTo :: Request -> Log.LoggingT IO (Maybe Response)
            respondTo req = buildResponse (respond req) req

            cancelReq :: ErrorObj -> BatchRequest -> Log.LoggingT IO ()
            cancelReq err = \case
                SingleRequest req@Request{} -> do
                    let reqVersion = getReqVer req
                        reqId = getReqId req
                    sendResponses $ SingleResponse $ ResponseError reqVersion err reqId
                SingleRequest Notif{} -> pure ()
                BatchRequest reqs -> do
                    sendResponses $ BatchResponse $ [ResponseError (getReqVer req) err (getReqId req) | req <- reqs, isRequest req]

            processReq :: BatchRequest -> Log.LoggingT IO ()
            processReq = \case
                SingleRequest req -> do
                    rM <- respondTo req
                    mapM_ (sendResponses . SingleResponse) rM
                BatchRequest reqs -> do
                    rs <- catMaybes <$> mapM respondTo reqs
                    sendResponses $ BatchResponse rs


            catchesHandler a e = foldr tryHandler (throw e) $ JsonRpcHandler (\(_ :: ReqException) -> pure cancelError) : handlers
                where tryHandler (JsonRpcHandler handler) res
                        = case fromException e of
                            Just e' -> 
                                flip Log.runLoggingT logger $ do
                                    err <- handler e'
                                    cancelReq err a
                            Nothing -> res

            bracketOnReqException before thing =
                mask $ \restore -> do
                    a <- before
                    restore (thing a) `catch` catchesHandler a

        liftIO $
            forkIO $
                forever $
                    bracketOnReqException
                        (atomically $ readTChan reqQueue)
                        (withLog . processReq)