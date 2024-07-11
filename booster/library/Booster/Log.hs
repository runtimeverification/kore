{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Booster.Log (
    ContextFor (..),
    LogMessage (..),
    Logger (..),
    LoggerT (..),
    LoggerMIO (..),
    ToLogFormat (..),
    WithJsonMessage (..),
    logMessage,
    logMessage',
    logPretty,
    logPretty',
    filterLogger,
    jsonLogger,
    textLogger,
    withContext,
    withContext_,
    withContexts,
    withKorePatternContext,
    withPatternContext,
    withRuleContext,
    withTermContext,
    -- re-export SimpleContext for withContext
    SimpleContext (..),
) where

import Control.Arrow (first)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Logger qualified
import Control.Monad.Trans.Class qualified as Trans
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.Reader (ReaderT (..), ask, withReaderT)
import Control.Monad.Trans.State (StateT (..))
import Control.Monad.Trans.State.Strict qualified as Strict
import Data.Aeson (ToJSON (..), Value (..), (.=))
import Data.Aeson.Encode.Pretty (Config (confIndent), Indent (Spaces), encodePretty')
import Data.Aeson.Types (object)
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.Hashable qualified
import Data.List (foldl', intercalate, intersperse)
import Data.List.Extra (splitOn, takeEnd)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Network.JSONRPC qualified as JSONRPC
import Prettyprinter (Pretty, pretty)
import System.Log.FastLogger (FormattedTime)
import UnliftIO (MonadUnliftIO)

import Booster.Definition.Attributes.Base
import Booster.Definition.Base (RewriteRule (..), SourceRef (..), sourceRef)
import Booster.Pattern.Base (
    Pattern (..),
    Predicate (..),
    Symbol (..),
    Term (..),
    TermAttributes (hash),
    pattern AndTerm,
 )
import Booster.Pattern.Pretty
import Booster.Prettyprinter (renderOneLineText)
import Booster.Syntax.Json (KorePattern, addHeader, prettyPattern)
import Booster.Syntax.Json.Externalise (externaliseTerm)
import Booster.Util (Flag (..))
import Kore.JsonRpc.Types (rpcJsonConfig)
import Kore.JsonRpc.Types.ContextLog as CL
import Kore.Util

newtype Logger a = Logger (a -> IO ())

class ToLogFormat a where
    toTextualLog :: a -> Text
    toJSONLog :: a -> Value

instance ToLogFormat CLContext where
    toTextualLog = pack . show
    toJSONLog = toJSON

instance ToLogFormat Text where
    toTextualLog t = t
    toJSONLog t = String t

instance ToLogFormat String where
    toTextualLog = pack
    toJSONLog = String . pack

instance ToLogFormat (PrettyWithModifiers mods Term) where
    toTextualLog t = renderOneLineText $ pretty t
    toJSONLog (PrettyWithModifiers t) = toJSON $ addHeader $ externaliseTerm t

instance ToLogFormat (RewriteRule tag) where
    toTextualLog = shortRuleLocation
    toJSONLog = String . shortRuleLocation

shortRuleLocation :: RewriteRule tag -> Text
shortRuleLocation rule = renderOneLineText $
    pretty $
        case sourceRef rule of
            Located l@Location{file = FileSource f} ->
                Located l{file = FileSource $ "..." <> (intercalate "/" $ takeEnd 3 $ splitOn "/" f)}
            loc -> loc

data LogMessage where
    LogMessage :: ToLogFormat a => Flag "alwaysShown" -> [CLContext] -> a -> LogMessage

class MonadIO m => LoggerMIO m where
    getLogger :: m (Logger LogMessage)
    default getLogger :: (Trans.MonadTrans t, LoggerMIO n, m ~ t n) => m (Logger LogMessage)
    getLogger = Trans.lift getLogger

    getPrettyModifiers :: m ModifiersRep
    default getPrettyModifiers :: (Trans.MonadTrans t, LoggerMIO n, m ~ t n) => m ModifiersRep
    getPrettyModifiers = Trans.lift getPrettyModifiers

    withLogger :: (Logger LogMessage -> Logger LogMessage) -> m a -> m a

instance LoggerMIO m => LoggerMIO (MaybeT m) where
    withLogger modL (MaybeT m) = MaybeT $ withLogger modL m
instance LoggerMIO m => LoggerMIO (ExceptT e m) where
    withLogger modL (ExceptT m) = ExceptT $ withLogger modL m
instance LoggerMIO m => LoggerMIO (ReaderT r m) where
    withLogger modL (ReaderT m) = ReaderT $ \r -> withLogger modL $ m r
instance LoggerMIO m => LoggerMIO (StateT s m) where
    withLogger modL (StateT m) = StateT $ \s -> withLogger modL $ m s
instance LoggerMIO m => LoggerMIO (Strict.StateT s m) where
    withLogger modL (Strict.StateT m) = Strict.StateT $ \s -> withLogger modL $ m s

instance MonadIO m => LoggerMIO (Control.Monad.Logger.NoLoggingT m) where
    getLogger = pure $ Logger $ \_ -> pure ()
    getPrettyModifiers = pure $ ModifiersRep @'[Decoded, Truncated] Proxy
    withLogger _ = id

logMessage :: (LoggerMIO m, ToLogFormat a) => a -> m ()
logMessage a =
    getLogger >>= \case
        (Logger l) -> liftIO $ l $ LogMessage (Flag False) [] a

{- Log message which is always shown even when context filters are applied -}
logMessage' :: (LoggerMIO m, ToLogFormat a) => a -> m ()
logMessage' a =
    getLogger >>= \case
        (Logger l) -> liftIO $ l $ LogMessage (Flag True) [] a

logPretty :: (LoggerMIO m, Pretty a) => a -> m ()
logPretty = logMessage . renderOneLineText . pretty

logPretty' ::
    forall mods m a.
    (LoggerMIO m, Pretty (PrettyWithModifiers mods a), FromModifiersT mods) =>
    a ->
    m ()
logPretty' = logMessage . renderOneLineText . pretty' @mods

withContext :: LoggerMIO m => SimpleContext -> m a -> m a
withContext c = withContext_ (CLNullary c)

withContexts :: LoggerMIO m => [SimpleContext] -> m a -> m a
withContexts [] m = m
withContexts cs m = foldr (withContext) m cs

withContext_ :: LoggerMIO m => CLContext -> m a -> m a
withContext_ c =
    withLogger
        ( \(Logger l) -> Logger $ l . (\(LogMessage alwaysShown ctxt m) -> LogMessage alwaysShown (c : ctxt) m)
        )

withTermContext :: LoggerMIO m => Term -> m a -> m a
withTermContext t@(Term attrs _) m =
    withContext_ (CLWithId . CtxTerm . ShortId $ showHashHex attrs.hash) $ do
        withContext CtxKoreTerm $
            getPrettyModifiers >>= \case
                ModifiersRep (_ :: FromModifiersT mods => Proxy mods) ->
                    logMessage (PrettyWithModifiers @mods t)
        m

withPatternContext :: LoggerMIO m => Pattern -> m a -> m a
withPatternContext Pattern{term, constraints} m =
    let t' = foldl' AndTerm term $ Set.toList $ Set.map coerce constraints -- FIXME
     in withTermContext t' m

instance ToLogFormat KorePattern where
    toTextualLog = prettyPattern
    toJSONLog p = toJSON p

withKorePatternContext :: LoggerMIO m => KorePattern -> m a -> m a
withKorePatternContext p m =
    withContextFor p $ do
        withContext CtxKoreTerm $ logMessage p
        m

withRuleContext ::
    ContextFor (RewriteRule tag) =>
    LoggerMIO m =>
    RewriteRule tag ->
    m a ->
    m a
withRuleContext rule m = withContextFor rule $ do
    withContext CtxDetail $ logPretty $ case sourceRef rule of
        Located Location{file = FileSource f, position} ->
            Located
                Location{file = FileSource $ "..." <> (intercalate "/" $ takeEnd 3 $ splitOn "/" f), position}
        loc -> loc
    m

class ContextFor a where
    withContextFor :: LoggerMIO m => a -> m b -> m b

instance ContextFor Term where
    withContextFor (Term attrs _) =
        withContext_ (CLWithId . CtxTerm . ShortId $ showHashHex attrs.hash)

instance ContextFor KorePattern where
    withContextFor p =
        withContext_ (CLWithId . CtxTerm . ShortId . showHashHex . Data.Hashable.hash $ show p) -- FIXME

instance ContextFor (RewriteRule "Rewrite") where
    withContextFor r =
        withContext_ (CLWithId . CtxRewrite $ parseRuleId r)

instance ContextFor (RewriteRule "Function") where
    withContextFor r =
        withContext_ (CLWithId . CtxFunction $ parseRuleId r)

instance ContextFor (RewriteRule "Simplification") where
    withContextFor r =
        withContext_ (CLWithId . CtxSimplification $ parseRuleId r)

instance ContextFor (RewriteRule "Ceil") where
    withContextFor r =
        withContext_ (CLWithId . CtxCeil $ parseRuleId r)

instance ContextFor Symbol where
    withContextFor s =
        withContext_ (CLWithId . CtxHook $ maybe "not-hooked" decodeUtf8 s.attributes.hook)

instance ContextFor JSONRPC.Id where
    withContextFor r =
        withContext_ (CLWithId . CtxRequest $ pack $ JSONRPC.fromId r)

parseRuleId :: RewriteRule tag -> CL.UniqueId
parseRuleId = fromMaybe CL.UNKNOWN . CL.parseUId . coerce . (.attributes.uniqueId)

data WithJsonMessage where
    WithJsonMessage :: ToLogFormat a => Value -> a -> WithJsonMessage

instance ToLogFormat WithJsonMessage where
    toTextualLog (WithJsonMessage _ a) = toTextualLog a
    toJSONLog (WithJsonMessage v _) = v

newtype LoggerT m a = LoggerT {unLoggerT :: ReaderT (Logger LogMessage, ModifiersRep) m a}
    deriving newtype
        ( Applicative
        , Functor
        , Monad
        , MonadIO
        , Control.Monad.Logger.MonadLogger
        , Control.Monad.Logger.MonadLoggerIO
        , MonadUnliftIO
        )

instance MonadIO m => LoggerMIO (LoggerT m) where
    getLogger = LoggerT (fst <$> ask)
    getPrettyModifiers = LoggerT (snd <$> ask)
    withLogger modL (LoggerT m) = LoggerT $ withReaderT (first modL) m

textLogger :: ((Maybe FormattedTime -> Control.Monad.Logger.LogStr) -> IO ()) -> Logger LogMessage
textLogger l = Logger $ \(LogMessage _ ctxts msg) ->
    let logLevel = mconcat $ intersperse "][" $ map toTextualLog ctxts
     in l $ \mTime ->
            ( case mTime of
                Nothing -> mempty
                Just t -> Control.Monad.Logger.toLogStr t <> " "
            )
                <> "["
                <> (Control.Monad.Logger.toLogStr logLevel)
                <> "] "
                <> (Control.Monad.Logger.toLogStr $ toTextualLog msg)
                <> "\n"

jsonLogger :: ((Maybe FormattedTime -> Control.Monad.Logger.LogStr) -> IO ()) -> Logger LogMessage
jsonLogger l = Logger $ \(LogMessage _ ctxts msg) ->
    let ctxt = toJSON $ map toJSONLog ctxts
     in liftIO $
            l $ \mTime ->
                ( Control.Monad.Logger.toLogStr $
                    encodePretty' rpcJsonConfig{confIndent = Spaces 0} $
                        object $
                            ["context" .= ctxt, "message" .= toJSONLog msg]
                                <> case mTime of
                                    Nothing -> []
                                    Just t -> ["timestamp" .= decodeUtf8 t]
                )
                    <> "\n"

filterLogger :: (LogMessage -> Bool) -> Logger LogMessage -> Logger LogMessage
filterLogger p (Logger l) = Logger $ \m -> when (p m) $ l m
