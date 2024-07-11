{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2023
License     : BSD-3-Clause
-}
module Kore.Log.WarnStepTimeout (
    WarnStepTimeout (..),
    warnStepManualTimeout,
    warnStepMATimeout,
) where

import Log
import Prelude.Kore
import Pretty (Pretty)
import Pretty qualified

-- | @WarnStepTimeout@ is emitted when a step timed out.
data WarnStepTimeout
    = WarnStepManualTimeout Int
    | WarnStepMATimeout Int
    deriving stock (Show)

instance Pretty WarnStepTimeout where
    pretty (WarnStepManualTimeout st) =
        Pretty.hsep
            [ "Step timed out. Timeout is manually set to"
            , Pretty.pretty st
            , "milliseconds."
            ]
    pretty (WarnStepMATimeout st) =
        Pretty.hsep
            [ "Step timed out. Moving average of previous steps is "
            , Pretty.pretty st
            , "milliseconds."
            ]

instance Entry WarnStepTimeout where
    entrySeverity _ = Warning

    oneLineDoc _ = "WarnStepTimeout"

    oneLineContextDoc _ = single CtxWarn

    helpDoc _ = "warn when a step has timed out"

warnStepManualTimeout :: MonadLog log => Int -> log ()
warnStepManualTimeout = logEntry . WarnStepManualTimeout . toMilliSec

warnStepMATimeout :: MonadLog log => Int -> log ()
warnStepMATimeout = logEntry . WarnStepMATimeout . toMilliSec

toMilliSec :: Int -> Int
toMilliSec t = t `div` 1000
