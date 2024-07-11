{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnSymbolSMTRepresentation (
    WarnSymbolSMTRepresentation (..),
    warnSymbolSMTRepresentation,
) where

import GHC.Generics as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Symbol (
    getSmthook,
    getSmtlib,
    sourceLocation,
 )
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Internal.TermLike
import Kore.Unparser (
    unparse,
 )
import Log
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified
import SQL qualified

newtype WarnSymbolSMTRepresentation = WarnSymbolSMTRepresentation {symbol :: Symbol}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic, Typeable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Pretty WarnSymbolSMTRepresentation where
    pretty WarnSymbolSMTRepresentation{symbol} =
        Pretty.vsep
            [ "Cannot translate symbol despite being given an SMT-LIB expression"
            , Pretty.indent 4 (unparse symbol)
            ]

instance Entry WarnSymbolSMTRepresentation where
    entrySeverity _ = Warning
    oneLineDoc (WarnSymbolSMTRepresentation Symbol{symbolAttributes}) =
        Pretty.pretty (sourceLocation symbolAttributes)
    oneLineContextDoc _ = map CLNullary [CtxSMT, CtxWarn]
    helpDoc _ =
        "warn when a symbol cannot be translated for the SMT solver, despite being given an explicit translation"

instance SQL.Table WarnSymbolSMTRepresentation

warnSymbolSMTRepresentation :: MonadLog m => Symbol -> m ()
warnSymbolSMTRepresentation
    symbol@Symbol{symbolAttributes = Attribute.Symbol{smtlib, smthook}}
        | (isJust . getSmtlib) smtlib || (isJust . getSmthook) smthook =
            logEntry WarnSymbolSMTRepresentation{symbol}
        | otherwise = return ()
