{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.DebugProven (
    DebugProven (..),
) where

import Data.Aeson qualified as JSON
import Kore.Attribute.SourceLocation (SourceLocation)
import Kore.Reachability.SomeClaim (
    SomeClaim (..),
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Pretty qualified

newtype DebugProven = DebugProven {claim :: SomeClaim}
    deriving stock (Show)

instance Pretty DebugProven where
    pretty DebugProven{claim} =
        Pretty.vsep ["Proved claim:", Pretty.indent 4 (pretty claim)]

instance Entry DebugProven where
    entrySeverity _ = Debug
    oneLineDoc DebugProven{claim} = pretty @SourceLocation $ from claim
    oneLineJson entry =
        JSON.object ["entry" JSON..= Log.entryTypeText (Log.toEntry entry)]
    helpDoc _ = "log proven claims"
