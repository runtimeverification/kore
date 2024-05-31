{-# OPTIONS_GHC -Wno-partial-fields #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Kore.JsonRpc.Types.ContextLog (
    module Kore.JsonRpc.Types.ContextLog,
) where

import Data.Aeson.Types (FromJSON (..), ToJSON (..), (.:))
import Data.Aeson.Types qualified as JSON
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
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
    | CtxRewrite Hex
    | CtxSimplification Hex
    | CtxFunction Hex
    | CtxTerm Hex
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
    toJSON = JSON.genericToJSON options -- FIXME needs manual adjustments

instance FromJSON CLContext where
    parseJSON = JSON.genericParseJSON options -- FIXME needs manual adjustments

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
      , message :: CLTextOrTerm
      }
    deriving stock (Generic, Show, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON '[] LogLine

data CLTextOrTerm
    = CLText Text
    | CLTerm KoreJson
    deriving stock (Generic, Show, Eq)

-- a message is a term if it is an object with format: KORE
instance FromJSON CLTextOrTerm where
    parseJSON obj@(JSON.Object o) = do
        _ :: KORE <- o .: "format" -- must be KORE
        CLTerm <$> parseJSON obj
    parseJSON (JSON.String msg) =
        pure $ CLText msg
    parseJSON other =
        JSON.typeMismatch "KoreJson object or string" other

instance ToJSON CLTextOrTerm where
    toJSON (CLText text) = toJSON text
    toJSON (CLTerm term) = toJSON term

newtype Hex = Hex Int
    deriving stock (Generic, Eq, Ord)

instance Show Hex where
    show (Hex i) = showHex i ""

instance FromJSON Hex where
    parseJSON = JSON.withText "Hexadecimal Hash" parseHex
      where
        parseHex :: Text -> JSON.Parser Hex
        parseHex hex =
            case readHex $ unpack hex of
                [(h, "")] -> pure  $ Hex h
                _otherwise -> JSON.parseFail $ "Bad hash value: " <> show hex

instance ToJSON Hex where
    toJSON = toJSON . show
