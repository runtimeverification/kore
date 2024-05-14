{-# OPTIONS_GHC -Wno-partial-fields #-}

module Kore.JsonRpc.Types.Log (module Kore.JsonRpc.Types.Log) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Text (encodeToLazyText)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Lazy qualified as Text (toStrict)
import GHC.Generics (Generic)
import Kore.JsonRpc.Types.Depth (Depth (..))

import Deriving.Aeson (
    CamelToKebab,
    ConstructorTagModifier,
    CustomJSON (..),
    FieldLabelModifier,
    OmitNothingFields,
    StripPrefix,
 )

data LogOrigin = KoreRpc | Booster | Llvm | Proxy
    deriving stock (Generic, Show, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[OmitNothingFields, FieldLabelModifier '[CamelToKebab], ConstructorTagModifier '[CamelToKebab]]
                LogOrigin

data LogRewriteResult
    = Success
        { ruleId :: !Text
        }
    | Failure
        { reason :: !Text
        , _ruleId :: !(Maybe Text)
        }
    deriving stock (Generic, Show, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[ OmitNothingFields
                 , FieldLabelModifier '[CamelToKebab, StripPrefix "_"]
                 , ConstructorTagModifier '[CamelToKebab]
                 ]
                LogRewriteResult

data LogEntry
    = Rewrite
        { result :: !LogRewriteResult
        , origin :: !LogOrigin
        }
    | Simplification
        { result :: !LogRewriteResult
        , origin :: !LogOrigin
        }
    | -- | Indicates a fallback of an RPC-server to a more powerful, but slower backup server, i.e. Booster to Kore
      Fallback
        { reason :: !Text
        -- ^ fallback reason
        , fallbackRuleId :: !Text
        -- ^ the rule that caused the fallback
        , recoveryRuleIds :: !(Maybe (NonEmpty Text))
        -- ^ rules applied during fallback, the first rule is the one that caused the fallback
        , recoveryDepth :: !Depth
        -- ^ depth reached in fallback, must be the same as (length ruleIds)
        , origin :: !LogOrigin
        -- ^ proxy server the log was emitted from
        }
    | -- | summatory timing, by origin or overall
      ProcessingTime
        { component :: Maybe LogOrigin
        , time :: Double
        }
    deriving stock (Generic, Show, Eq)
    deriving
        (FromJSON, ToJSON)
        via CustomJSON
                '[OmitNothingFields, FieldLabelModifier '[CamelToKebab], ConstructorTagModifier '[CamelToKebab]]
                LogEntry

-- | If the log entry contains the any of the term fields, make them Nothing
logEntryEraseTerms :: LogEntry -> LogEntry
logEntryEraseTerms = id

-- | Encode a Kore RPC as Text-embedded JSON for stderr/file logging
encodeLogEntryText :: LogEntry -> Text
encodeLogEntryText = Text.toStrict . encodeToLazyText
