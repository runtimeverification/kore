{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Log.BoosterAdaptor (
    renderStandardPretty,
    renderOnelinePretty,
    renderJson,
    koreSomeEntryLogAction,
    withLogger,
    WithTimestamp (..),
    module KoreLogOptions,
    module Log,
    ExeName (..),
) where

import Control.Monad.Cont (
    ContT (..),
    runContT,
 )
import Data.Aeson qualified as JSON
import Data.Aeson.Encode.Pretty qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vec
import Pretty qualified

import Control.Monad.Logger (LogStr, ToLogStr (toLogStr))
import Data.Aeson.Encode.Pretty (Config (confIndent), Indent (Spaces))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Kore.JsonRpc.Types (rpcJsonConfig)
import Kore.Log (WithTimestamp (..), swappableLogger, withTimestamp)
import Kore.Log qualified as Log
import Kore.Log.KoreLogOptions as KoreLogOptions
import Kore.Log.Registry (
    lookupTextFromTypeWithError,
    typeOfSomeEntry,
 )
import Log
import Prelude.Kore

withLogger ::
    KoreLogOptions ->
    (LogAction IO SomeEntry -> IO a) ->
    IO a
withLogger koreLogOptions = runContT $ do
    mainLogger <- ContT $ withMainLogger koreLogOptions
    let KoreLogOptions{exeName, debugSolverOptions} = koreLogOptions
    smtSolverLogger <- ContT $ Log.withSmtSolverLogger exeName debugSolverOptions
    return $ mainLogger <> smtSolverLogger

withMainLogger ::
    KoreLogOptions ->
    (LogAction IO SomeEntry -> IO a) ->
    IO a
withMainLogger koreLogOptions = runContT $ do
    pure
        . Log.koreLogTransformer koreLogOptions
        . Log.koreLogFilters koreLogOptions
        $ case logType koreLogOptions of
            LogProxy logAction -> logAction
            ltype -> error ("Unexpected log type " <> show ltype)

koreSomeEntryLogAction ::
    MonadIO m =>
    -- | how to render a timestamped 'SomeEntry' into Text
    (Maybe ByteString -> SomeEntry -> LogStr) ->
    -- | filter log entries, applies BEFORE rendering to text
    (SomeEntry -> Bool) ->
    ((Maybe ByteString -> LogStr) -> IO ()) ->
    LogAction m SomeEntry
koreSomeEntryLogAction renderer earlyFilter logger =
    LogAction $ \se -> liftIO $
        when (earlyFilter se) $
            logger $
                \mTime -> renderer mTime se <> "\n"

renderJson :: Maybe ByteString -> SomeEntry -> LogStr
renderJson mTime e@(SomeEntry context actualEntry) =
    toLogStr . Text.decodeUtf8 . BSL.toStrict . JSON.encodePretty' rpcJsonConfig{confIndent = Spaces 0} $
        json
  where
    jsonContext =
        Vec.fromList $
            concatMap (\(SomeEntry _ c) -> map JSON.toJSON (oneLineContextDoc c)) (context <> [e])

    json = case oneLineJson actualEntry of
        JSON.Object o
            | Just (JSON.Array ctxt) <- JSON.lookup "context" o ->
                JSON.Object
                    $ ( case mTime of
                            Nothing -> id
                            Just time -> JSON.insert "timestamp" (JSON.String $ Text.decodeUtf8 time)
                      )
                    $ JSON.insert "context" (JSON.Array $ jsonContext <> ctxt) o
        other ->
            JSON.object $
                [ "message" JSON..= other
                , "context" JSON..= jsonContext
                ]
                    <> case mTime of
                        Nothing -> []
                        Just time -> ["timestamp" JSON..= Text.decodeUtf8 time]

renderOnelinePretty :: Maybe ByteString -> SomeEntry -> LogStr
renderOnelinePretty mTime entry@(SomeEntry entryContext _actualEntry) =
    let cs =
            (entryContext <> [entry])
                & concatMap (map Pretty.brackets . (\(SomeEntry _ e) -> Pretty.pretty . show <$> oneLineContextDoc e))
        leaf = oneLineDoc entry
     in maybe mempty ((<> " ") . toLogStr) mTime
            <> ( mconcat cs Pretty.<+> leaf
                    & Pretty.layoutPretty Pretty.defaultLayoutOptions{Pretty.layoutPageWidth = Pretty.Unbounded}
                    & Pretty.renderText
                    & toLogStr
               )

renderStandardPretty :: Maybe ByteString -> SomeEntry -> LogStr
renderStandardPretty mTime entry@(SomeEntry entryContext actualEntry) =
    prettyActualEntry
        & Pretty.layoutPretty Pretty.defaultLayoutOptions
        & Pretty.renderText
        & toLogStr
  where
    timestamp =
        case mTime of
            Nothing -> Nothing
            Just time ->
                Just $ Pretty.brackets $ Pretty.pretty $ Text.decodeUtf8 time
    prettyActualEntry =
        Pretty.vsep . concat $
            [ [header]
            , indent <$> [longDoc actualEntry]
            , context'
            ]
      where
        header =
            (Pretty.hsep . catMaybes)
                [ timestamp
                , Just severity'
                , Just (Pretty.parens $ type' entry)
                ]
                <> Pretty.colon
        severity' = prettySeverity (entrySeverity actualEntry)
        type' e =
            Pretty.pretty $
                lookupTextFromTypeWithError $
                    typeOfSomeEntry e
        context' =
            (entry : entryContext)
                & reverse
                & mapMaybe (\e -> (,type' e) <$> contextDoc e)
                & prettyContext
        prettyContext =
            \case
                [] -> []
                xs -> ("Context" <> Pretty.colon) : (indent <$> mkContext xs)

        mkContext =
            \case
                [] -> []
                [(doc, typeName)] ->
                    [Pretty.hsep [Pretty.parens typeName, doc]]
                (doc, typeName) : xs -> (Pretty.hsep [Pretty.parens typeName, doc]) : (indent <$> (mkContext xs))

    indent = Pretty.indent 4
