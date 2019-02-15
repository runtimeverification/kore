{-|
Module      : Kore.Logger
Description : Logging functions.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : vladimir.ciobanu@runtimeverification.com
Stability   : experimental
Portability : portable
-}

module Kore.Logger
    ( LogMessage (..)
    , Severity (..)
    , log
    , logDebug
    , logInfo
    , logWarning
    , logError
    , logCritical
    , withLogScope
    ) where

import           Colog
                 ( WithLog )
import qualified Colog as Colog
import           Data.Functor.Contravariant
                 ( contramap )
import           Data.Text
                 ( Text )
import qualified Data.Text as Text
import           Prelude hiding
                 ( log )

-- | Log level used to describe each log message. It is also used to set the
-- minimum level to be outputted.
data Severity
    = Debug
    -- ^ Lowest level used for low-level debugging.
    | Info
    -- ^ Used for various informative messages.
    | Warning
    -- ^ Used for odd/unusual cases which are recoverable.
    | Error
    -- ^ Used for application errors, unexpected behaviors, etc.
    | Critical
    -- ^ Used before shutting down the application.
    deriving (Show, Read, Eq, Ord)

-- | This type should not be used directly, but rather should be created and
-- dispatched through the `log` functions.
data LogMessage = LogMessage
    { lmMessage  :: !Text
    -- ^ message being logged
    , lmSeverity :: !Severity
    -- ^ log level / severity of message
    , lmScope    :: !Text
    -- ^ scope of the message, usually of the form "a.b.c"
    }

-- | Logs a message using given 'Severity'.
log
    :: forall env m
    . (WithLog env LogMessage m)
    => Severity
    -- ^ If lower than the minimum severity, the message will not be logged
    -> Text
    -- ^ Message to be logged
    -> m ()
log s t = Colog.logMsg $ LogMessage t s ""

-- | Logs using 'Debug' log level. See 'log'.
logDebug
    :: forall env m
    . (WithLog env LogMessage m)
    => Text
    -> m ()
logDebug t = Colog.logMsg $ LogMessage t Debug ""

-- | Logs using 'Info' log level. See 'log'.
logInfo
    :: forall env m
    . (WithLog env LogMessage m)
    => Text
    -> m ()
logInfo t = Colog.logMsg $ LogMessage t Info ""

-- | Logs using 'Warning' log level. See 'log'.
logWarning
    :: forall env m
    . (WithLog env LogMessage m)
    => Text
    -> m ()
logWarning t = Colog.logMsg $ LogMessage t Warning ""

-- | Logs using 'Error' log level. See 'log'.
logError
    :: forall env m
    . (WithLog env LogMessage m)
    => Text
    -> m ()
logError t = Colog.logMsg $ LogMessage t Error ""

-- | Logs using 'Critical' log level. See 'log'.
logCritical
    :: forall env m
    . (WithLog env LogMessage m)
    => Text
    -> m ()
logCritical t = Colog.logMsg $ LogMessage t Critical ""

-- | Creates a new logging scope, appending the text to the current scope. For
-- example, if the current scope is "a.b" and 'withLogScope' is called with
-- "c", then the new scope will be "a.b.c".
withLogScope
    :: forall env m a
    .  WithLog env LogMessage m
    => Text
    -- ^ new scope
    -> m a
    -- ^ continuation / enclosure for the new scope
    -> m a
withLogScope newScope = Colog.withLog (contramap appendScope)
  where
    appendScope (LogMessage msg sev scope) =
        LogMessage msg sev $
            if Text.null scope
               then newScope
               else (newScope <> "." <> scope)
