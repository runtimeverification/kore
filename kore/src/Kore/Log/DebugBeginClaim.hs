{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugBeginClaim (
    DebugBeginClaim (..),
    debugBeginClaim,
) where

import Kore.Attribute.SourceLocation (
    SourceLocation,
 )
import Kore.Reachability.SomeClaim (
    SomeClaim (..),
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

newtype DebugBeginClaim = DebugBeginClaim {claim :: SomeClaim}
    deriving stock (Show)

instance Pretty DebugBeginClaim where
    pretty DebugBeginClaim{claim} =
        Pretty.vsep ["Begin proof of claim:", Pretty.indent 4 (pretty claim)]

instance Entry DebugBeginClaim where
    entrySeverity _ = Debug
    helpDoc _ = "log starting claims"
    oneLineDoc DebugBeginClaim{claim} =
        pretty @SourceLocation $ from claim
    oneLineContextDoc _ = single CtxDetail

debugBeginClaim ::
    MonadLog log =>
    SomeClaim ->
    log ()
debugBeginClaim claim = logEntry (DebugBeginClaim claim)
