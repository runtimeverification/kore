{-# LANGUAGE TemplateHaskell #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Kore.JsonRpc.Types.ContextLog (
    module Kore.JsonRpc.Types.ContextLog,
) where

import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (FromJSON (..), ToJSON (..), defaultOptions)
import Data.Aeson.Types qualified as JSON
import Data.Data (Data, toConstr)
import Data.Sequence (Seq)
import Data.Text (Text, unpack)
import Data.Text qualified as Text
import Data.Time.Clock.System (SystemTime (..), systemToUTCTime, utcToSystemTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Text.Read (readMaybe)

data SimpleContext
    = -- component
      CtxBooster
    | CtxKore
    | CtxProxy
    | -- method
      CtxExecute
    | CtxSimplify
    | CtxImplies
    | CtxGetModel
    | CtxAddModule
    | -- mode/phase
      CtxInternalise
    | CtxMatch
    | CtxUnify
    | CtxDefinedness
    | CtxConstraint
    | CtxSMT
    | CtxLlvm
    | -- results
      CtxFailure
    | CtxIndeterminate
    | CtxAbort
    | CtxSuccess
    | CtxBreak
    | CtxContinue
    | -- information
      CtxKoreTerm
    | CtxDetail
    | CtxSubstitution
    | CtxRemainder
    | CtxDepth
    | CtxDebugRewriter
    | CtxTiming
    | -- standard log levels
      CtxError
    | CtxWarn
    | CtxInfo
    deriving stock (Eq, Ord, Enum, Data)

instance Show SimpleContext where
    show = JSON.camelTo2 '-' . drop 3 . show . toConstr

$( deriveJSON
    defaultOptions
        { JSON.constructorTagModifier = JSON.camelTo2 '-' . drop 3
        , JSON.sumEncoding = JSON.ObjectWithSingleField
        }
    ''SimpleContext
 )

----------------------------------------
data IdContext
    = -- entities with hex identifier
      CtxRewrite UniqueId
    | CtxSimplification UniqueId
    | CtxFunction UniqueId
    | CtxCeil UniqueId
    | CtxTerm UniqueId
    | -- entities with name
      CtxHook Text
    | CtxRequest Text
    | CtxCached Text
    deriving stock (Eq, Ord)

instance Show IdContext where
    show (CtxRewrite uid) = "rewrite " <> show uid
    show (CtxSimplification uid) = "simplification " <> show uid
    show (CtxFunction uid) = "function " <> show uid
    show (CtxCeil uid) = "ceil " <> show uid
    show (CtxTerm uid) = "term " <> show uid
    show (CtxHook name) = "hook " <> unpack name
    show (CtxRequest name) = "request " <> unpack name
    show (CtxCached name) = "cached " <> unpack name

getUniqueId :: IdContext -> Maybe UniqueId
getUniqueId = \case
    CtxRewrite uid -> Just uid
    CtxSimplification uid -> Just uid
    CtxFunction uid -> Just uid
    CtxCeil uid -> Just uid
    CtxTerm uid -> Just uid
    _ -> Nothing

----------------------------------------
data UniqueId
    = ShortId Text -- short hashes (7 char)
    | LongId Text -- long hashes (64 char)
    | UNKNOWN
    deriving stock (Eq, Ord)

instance Show UniqueId where
    show (ShortId x) = unpack x
    show (LongId x) = unpack $ Text.take 7 x -- for parity with legacy log
    show UNKNOWN = "UNKNOWN"

parseUId :: Text -> Maybe UniqueId
parseUId "UNKNOWN" = pure UNKNOWN
parseUId hex
    | Text.length hex == 7 = Just $ ShortId hex
    | Text.length hex == 64 = Just $ LongId hex
    | otherwise = Nothing

instance FromJSON UniqueId where
    parseJSON = JSON.withText "Hash ID" parseHex
      where
        parseHex :: Text -> JSON.Parser UniqueId
        parseHex hex =
            maybe (JSON.parseFail $ "Bad hash ID: " <> show hex) pure $ parseUId hex

instance ToJSON UniqueId where
    toJSON = \case
        ShortId x -> JSON.String x
        LongId x -> JSON.String x
        UNKNOWN -> JSON.String "UNKNOWN"

$( deriveJSON
    defaultOptions
        { JSON.constructorTagModifier = JSON.camelTo2 '-' . drop 3
        , JSON.sumEncoding = JSON.ObjectWithSingleField
        }
    ''IdContext
 )

----------------------------------------
data CLContext
    = CLNullary SimpleContext
    | CLWithId IdContext
    deriving stock (Eq, Ord)

instance Show CLContext where
    show (CLNullary c) = show c
    show (CLWithId withId) = show withId

instance ToJSON CLContext where
    toJSON (CLNullary c) = toJSON c
    toJSON (CLWithId withId) = toJSON withId

instance FromJSON CLContext where
    parseJSON = \case
        simple@JSON.String{} ->
            CLNullary <$> parseJSON simple
        obj@JSON.Object{} ->
            CLWithId <$> parseJSON obj
        other ->
            JSON.typeMismatch "Object or string" other

----------------------------------------
data CLMessage
    = CLText Text -- generic log message
    | CLValue JSON.Value -- other stuff
    deriving stock (Eq)

instance Show CLMessage where
    show (CLText t) = unpack t
    show (CLValue value) = show value

-- a message is a term if it is an object with format: KORE
instance FromJSON CLMessage where
    parseJSON (JSON.String msg) = pure $ CLText msg
    parseJSON obj@JSON.Object{} = pure $ CLValue obj
    parseJSON arr@JSON.Array{} = pure $ CLValue arr
    parseJSON JSON.Null = pure $ CLValue JSON.Null
    parseJSON other =
        JSON.typeMismatch "Object, array, or string" other

instance ToJSON CLMessage where
    toJSON (CLText text) = toJSON text
    toJSON (CLValue value) = value

data LogLine = LogLine
    { timestamp :: Maybe SystemTime
    , context :: Seq CLContext
    , message :: CLMessage
    }
    deriving stock (Show, Eq)

instance FromJSON LogLine where
    parseJSON = JSON.withObject "LogLine" $ \l ->
        LogLine
            <$> (l .:? "timestamp" >>= parseTimestamp)
            <*> l .: "context"
            <*> l .: "message"

parseTimestamp :: Maybe JSON.Value -> JSON.Parser (Maybe SystemTime)
parseTimestamp Nothing = pure Nothing
parseTimestamp (Just x) =
    JSON.withScientific "numeric timestamp" (pure . Just . fromNumeric) x
        <|> JSON.withText "human-readable timestamp" fromString x
        <|> JSON.withText "nanosecond timestamp" fromNanos x
  where
    -- fromNumeric :: Scientific -> SystemTime
    fromNumeric n =
        let seconds = truncate n
            nanos = truncate $ 1e9 * (n - fromIntegral seconds) -- no leap seconds
         in MkSystemTime seconds nanos
    fromString s = do
        utc <- parseTimeM False defaultTimeLocale timestampFormat (Text.unpack s)
        pure . Just $ utcToSystemTime utc
    fromNanos s =
        case readMaybe (Text.unpack s) of
            Nothing -> fail $ "bad number " <> show s
            Just (n :: Integer) -> pure . Just $ fromNumeric (fromIntegral n :: Double)

instance ToJSON LogLine where
    toJSON LogLine{timestamp, context, message} =
        JSON.object $
            maybe
                []
                (\t -> ["timestamp" .= formatted t])
                timestamp
                <> ["context" .= context, "message" .= message]
      where
        formatted = formatTime defaultTimeLocale timestampFormat . systemToUTCTime

-- similar to the one used in Booster.Util, but not setting a length for the sub-second digits
timestampFormat :: String
timestampFormat = "%Y-%m-%dT%H:%M:%S%Q"
