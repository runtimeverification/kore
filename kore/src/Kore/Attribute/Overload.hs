{- |
Module      : Kore.Attribute.Overload
Description : Overloaded production attribute
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Overload (
    Overload (..),
    overloadId,
    overloadSymbol,
    overloadAttribute,
) where

import Data.Binary (Binary)
import Data.Monoid qualified as Monoid
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- TODO: deriving stock (Functor) ???

-- | @Overload@ represents the @overload@ attribute for symbols.
newtype Overload symbol = Overload {getOverload :: Maybe (symbol, symbol)}
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via (Monoid.First (symbol, symbol))

instance Default (Overload symbol) where
    def = mempty

-- | Kore identifier representing the @overload@ attribute symbol.
overloadId :: Id
overloadId = "overload"

-- | Kore symbol representing the @overload@ attribute.
overloadSymbol :: SymbolOrAlias
overloadSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = overloadId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @overload@ attribute.
overloadAttribute ::
    SymbolOrAlias ->
    SymbolOrAlias ->
    AttributePattern
overloadAttribute symbol1 symbol2 =
    attributePattern
        overloadSymbol
        [ attributePattern_ symbol1
        , attributePattern_ symbol2
        ]

instance ParseAttributes (Overload SymbolOrAlias) where
    parseAttribute = withApplication' parseApplication
      where
        parseApplication params args Overload{getOverload}
            | Just _ <- getOverload = failDuplicate'
            | otherwise = do
                Parser.getZeroParams params
                (arg1, arg2) <- Parser.getTwoArguments args
                symbol1 <- Parser.getSymbolOrAlias arg1
                symbol2 <- Parser.getSymbolOrAlias arg2
                return Overload{getOverload = Just (symbol1, symbol2)}
        withApplication' = Parser.withApplication overloadId
        failDuplicate' = Parser.failDuplicate overloadId

instance From symbol SymbolOrAlias => From (Overload symbol) Attributes where
    from =
        maybe def toAttribute . getOverload
      where
        toAttribute (symbol1, symbol2) =
            from @AttributePattern $
                overloadAttribute (from symbol1) (from symbol2)
