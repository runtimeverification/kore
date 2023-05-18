{- |
Module      : Kore.Attribute.Function
Description : Function symbol attribute
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Function (
    Function (..),
    functionId,
    functionSymbol,
    functionAttribute,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @Function@ represents the @function@ attribute for symbols.
newtype Function = Function {isDeclaredFunction :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup Function where
    (<>) (Function a) (Function b) = Function (a || b)

instance Monoid Function where
    mempty = Function False

instance Default Function where
    def = mempty

-- | Kore identifier representing the @function@ attribute symbol.
functionId :: Id
functionId = "function"

-- | Kore symbol representing the @function@ attribute.
functionSymbol :: SymbolOrAlias
functionSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = functionId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @function@ attribute.
functionAttribute :: AttributePattern
functionAttribute = attributePattern_ functionSymbol

instance ParseAttributes Function where
    parseAttribute = parseBoolAttribute functionId

instance From Function Attributes where
    from = toBoolAttributes functionAttribute
