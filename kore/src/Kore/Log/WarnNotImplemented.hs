{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnNotImplemented (
    WarnNotImplemented (..),
    warnNotImplemented,
) where

import Kore.Attribute.Symbol (sourceLocation)
import Kore.Internal.TermLike
import Kore.Unparser
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified

newtype WarnNotImplemented variable = WarnNotImplemented {notImplementedApp :: Application Symbol (TermLike variable)}
    deriving stock (Show)

instance InternalVariable variable => Pretty (WarnNotImplemented variable) where
    pretty (WarnNotImplemented app) =
        Pretty.vsep
            [ "The following application of a builtin function is not implemented:"
            , unparse app
            ]

instance InternalVariable variable => Entry (WarnNotImplemented variable) where
    entrySeverity _ = Warning
    oneLineDoc (WarnNotImplemented (Application Symbol{symbolAttributes} _)) =
        Pretty.pretty $ sourceLocation symbolAttributes
    oneLineContextDoc _ = single CtxWarn
    helpDoc _ = "warn when we try to evaluate a partial builtin function on unimplemented cases"

warnNotImplemented ::
    MonadLog log =>
    InternalVariable variable =>
    Application Symbol (TermLike variable) ->
    log ()
warnNotImplemented = logEntry . WarnNotImplemented
