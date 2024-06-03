{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Kore.JsonRpc.Types.ContextLog (
    module Kore.JsonRpc.Types.ContextLog,
) where

import Control.Applicative ((<|>))
import Data.Aeson.Types (FromJSON (..), ToJSON (..), (.:))
import Data.Aeson.Types qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Time
import Deriving.Aeson
import Numeric

import Kore.Syntax.Json.Types (KoreJson, KORE)

-- | result type
data ContextLog
    = CtxLog
      { context :: [CLContext]
      , entries :: [ContextLog]
      , timestamp :: Maybe TimeOfDay
      , duration :: Maybe DiffTime
      }
    | CLEntry
      { message :: Maybe Text
      , term :: Maybe KoreJson
      }
    deriving stock (Generic, Show, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[OmitNothingFields] ContextLog

data CLContext
    = CtxBooster
    | CtxKore
    -- "modes"
    | CtxExecute
    | CtxSimplify
    | CtxImplies
    | CtxGetModel
    | CtxMatch
    -- entities with hex identifier
    | CtxRewrite UniqueId
    | CtxSimplification UniqueId
    | CtxFunction UniqueId
    | CtxTerm UniqueId
    -- entities with name
    | CtxHook Text
    -- results
    | CtxFailure
    | CtxAbort
    | CtxSuccess
    -- information
    | CtxKoreTerm
    | CtxDetail
    -- standard log levels
    | CtxError
    | CtxWarn
    | CtxInfo
    -- free-form
    | Ctx Text
    deriving stock (Generic, Show, Eq)

instance ToJSON CLContext where
    -- nullary constructors encoded as simple strings
    toJSON = \case
        CtxBooster -> JSON.String $ camelToKebab "Booster"
        CtxKore -> JSON.String $ camelToKebab "Kore"
        CtxExecute -> JSON.String $ camelToKebab "Execute"
        CtxSimplify -> JSON.String $ camelToKebab "Simplify"
        CtxImplies -> JSON.String $ camelToKebab "Implies"
        CtxGetModel -> JSON.String $ camelToKebab "GetModel"
        CtxMatch -> JSON.String $ camelToKebab "Match"
        CtxFailure -> JSON.String $ camelToKebab "Failure"
        CtxAbort -> JSON.String $ camelToKebab "Abort"
        CtxSuccess -> JSON.String $ camelToKebab "Success"
        CtxKoreTerm -> JSON.String $ camelToKebab "KoreTerm"
        CtxDetail -> JSON.String $ camelToKebab "Detail"
        CtxError -> JSON.String $ camelToKebab "Error"
        CtxWarn -> JSON.String $ camelToKebab "Warn"
        CtxInfo -> JSON.String $ camelToKebab "Info"
        Ctx txt-> JSON.String txt
        -- entities with hex identifier
        other -> JSON.genericToJSON options other

instance FromJSON CLContext where
    parseJSON = \case
        JSON.String "booster" -> pure CtxBooster
        JSON.String "kore" -> pure CtxKore
        JSON.String "execute" -> pure CtxExecute
        JSON.String "simplify" -> pure CtxSimplify
        JSON.String "implies" -> pure CtxImplies
        JSON.String "get-model" -> pure CtxGetModel
        JSON.String "match" -> pure CtxMatch
        JSON.String "failure" -> pure CtxFailure
        JSON.String "abort" -> pure CtxAbort
        JSON.String "success" -> pure CtxSuccess
        JSON.String "kore-term" -> pure CtxKoreTerm
        JSON.String "detail" -> pure CtxDetail
        JSON.String "error" -> pure CtxError
        JSON.String "warn" -> pure CtxWarn
        JSON.String "info" -> pure CtxInfo
        JSON.String other -> pure $ Ctx other

        obj -> JSON.genericParseJSON options obj

camelToKebab :: String -> Text
camelToKebab = pack . JSON.camelTo2 '-'

options :: JSON.Options
options =
    JSON.defaultOptions
    { JSON.sumEncoding = JSON.ObjectWithSingleField
    , JSON.constructorTagModifier = \name ->
            JSON.camelTo2 '-' $ fromMaybe name $ stripPrefix "Ctx" name
    , JSON.allNullaryToStringTag = True
    }

data LogLine
    = LogLine
      { context :: [CLContext]
      , message :: CLMessage
      }
    deriving stock (Generic, Show, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[] LogLine

data CLMessage
    = CLText Text  -- generic log message
    | CLArray [Text] -- array of partial symbol names (ceil analysis)
    | CLTerm KoreJson -- kore-term
    | CLId JSON.Key UniqueId -- simplification/rewrite/match success
    deriving stock (Generic, Show, Eq)

-- a message is a term if it is an object with format: KORE
instance FromJSON CLMessage where
    parseJSON obj@(JSON.Object o) =
        parseTerm <|> parseId
      where
        parseTerm = do
            _ :: KORE <- o .: "format" -- must be KORE
            CLTerm <$> parseJSON obj
        parseId = case JSON.toList o of
            [(aKey, uniqueId)] -> CLId aKey <$> parseJSON uniqueId
            _other -> JSON.typeMismatch "Singleton object" obj
    parseJSON (JSON.String msg) =
        pure $ CLText msg
    parseJSON arr@JSON.Array{} =
        CLArray <$> parseJSON arr
    parseJSON other =
        JSON.typeMismatch "Object, array, or string" other

instance ToJSON CLMessage where
    toJSON (CLText text) = toJSON text
    toJSON (CLArray msgs) = toJSON msgs
    toJSON (CLTerm term) = toJSON term
    toJSON (CLId key uniqueId) = JSON.Object $ JSON.singleton key (toJSON uniqueId)

data UniqueId
    = Hex Integer
    | UNKNOWN
    deriving stock (Generic, Eq, Ord)

instance Show UniqueId where
    show (Hex i) = showHex i ""
    show UNKNOWN = "UNKNOWN"

instance FromJSON UniqueId where
    parseJSON = JSON.withText "Hexadecimal Hash" parseHex
      where
        parseHex :: Text -> JSON.Parser UniqueId
        parseHex "UNKNOWN" = pure UNKNOWN
        parseHex hex =
            case readHex $ unpack hex of
                [(h, "")] -> pure  $ Hex h
                _otherwise -> JSON.parseFail $ "Bad hash value: " <> show hex

instance ToJSON UniqueId where
    toJSON (Hex x) = JSON.String . pack $ showHex x ""
    toJSON UNKNOWN = JSON.String "UNKNOWN"
