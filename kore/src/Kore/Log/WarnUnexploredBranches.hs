{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2022
License     : BSD-3-Clause
-}
module Kore.Log.WarnUnexploredBranches (
    WarnUnexploredBranches (..),
    warnUnexploredBranches,
) where

import Log
import Numeric.Natural
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

{- | @WarnUnexploredBranches@ is emitted when a proof gets stuck. It
   indicates how many other branches in the proof were left unexplored
   (and may also be failing if explored).
-}
data WarnUnexploredBranches
    = WarnUnexploredBranches Natural
    deriving stock (Show)

instance Pretty WarnUnexploredBranches where
    pretty (WarnUnexploredBranches count) =
        Pretty.pretty count
            Pretty.<+> "branches were still unexplored when the action failed."

instance Entry WarnUnexploredBranches where
    entrySeverity _ = Warning
    oneLineDoc (WarnUnexploredBranches count) =
        Pretty.pretty count
            Pretty.<+> "branches were still unexplored when the action failed."
    oneLineContextDoc _ = single CtxWarn
    helpDoc _ =
        "indicate whether and how many unexplored branches existed when failing."

warnUnexploredBranches :: MonadLog log => Natural -> log ()
warnUnexploredBranches = logEntry . WarnUnexploredBranches
