{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.InfoUserLog (
    InfoUserLog (..),
    infoUserLog,
) where

import Data.Text
import Debug
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

newtype InfoUserLog = InfoUserLog {getUserLog :: Text}
    deriving stock (Eq, Ord, Show)

instance Debug InfoUserLog where
    debugPrec w = \_ -> Pretty.pretty . show $ w

instance Diff InfoUserLog where
    diffPrec = diffPrecEq

instance Pretty InfoUserLog where
    pretty (InfoUserLog userLog) =
        Pretty.pretty userLog

instance Entry InfoUserLog where
    entrySeverity _ = Info
    oneLineDoc (InfoUserLog userLog) = Pretty.pretty userLog
    oneLineContextDoc _ = single CtxInfo
    helpDoc _ = "user-specified log message"

infoUserLog :: MonadLog log => Text -> log ()
infoUserLog = logEntry . InfoUserLog
