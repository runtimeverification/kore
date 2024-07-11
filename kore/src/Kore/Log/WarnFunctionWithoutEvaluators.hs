{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Log.WarnFunctionWithoutEvaluators (
    WarnFunctionWithoutEvaluators (..),
    warnFunctionWithoutEvaluators,
) where

import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Symbol (sourceLocation)
import Kore.Attribute.Symbol qualified as Attribute (
    Symbol (..),
 )
import Kore.Internal.Symbol (
    Symbol (..),
    noEvaluators,
 )
import Kore.Unparser (
    unparse,
 )
import Log (
    Entry (..),
    MonadLog,
    Severity (Warning),
    SimpleContext (CtxWarn),
    logEntry,
    single,
 )
import Prelude.Kore
import Pretty (
    Pretty,
 )
import Pretty qualified
import SQL qualified

newtype WarnFunctionWithoutEvaluators = WarnFunctionWithoutEvaluators {symbol :: Symbol}
    deriving stock (Show, Eq, Typeable)
    deriving stock (GHC.Generic)

instance SOP.Generic WarnFunctionWithoutEvaluators

instance SOP.HasDatatypeInfo WarnFunctionWithoutEvaluators

instance Pretty WarnFunctionWithoutEvaluators where
    pretty WarnFunctionWithoutEvaluators{symbol} =
        let Symbol{symbolAttributes} = symbol
         in let Attribute.Symbol{klabel, sourceLocation} = symbolAttributes
             in Pretty.vsep
                    [ "No evaluators for function symbol:"
                    , Pretty.indent 4 $
                        Pretty.hsep
                            [ unparse symbol
                            , Pretty.parens $ Pretty.pretty klabel
                            ]
                    , Pretty.hsep
                        [ "defined at:"
                        , Pretty.pretty sourceLocation
                        ]
                    ]

instance Entry WarnFunctionWithoutEvaluators where
    entrySeverity _ = Warning
    oneLineDoc (WarnFunctionWithoutEvaluators Symbol{symbolAttributes}) =
        Pretty.pretty $ sourceLocation symbolAttributes
    oneLineContextDoc _ = single CtxWarn
    helpDoc _ = "warn when encountering a function with no definition"

instance SQL.Table WarnFunctionWithoutEvaluators

warnFunctionWithoutEvaluators :: MonadLog m => Symbol -> m ()
warnFunctionWithoutEvaluators symbol
    | noEvaluators symbol = return ()
    | otherwise = logEntry WarnFunctionWithoutEvaluators{symbol}
