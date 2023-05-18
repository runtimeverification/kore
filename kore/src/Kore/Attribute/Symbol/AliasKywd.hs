{- |
Copyright   : (c) Runtime Verification, 2019-2023
License     : BSD-3-Clause
-}
module Kore.Attribute.Symbol.AliasKywd (
    AliasKywd (..),
    aliasKywdId,
    aliasKywdSymbol,
    aliasKywdAttribute,
) where

import Data.Binary (Binary)
import Data.Monoid (
    Any (..),
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @AliasKywd@ represents the @aliasKywd@ attribute for symbols.
newtype AliasKywd = AliasKywd {isAliasKywd :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via Any

instance Default AliasKywd where
    def = mempty

-- | Kore identifier representing the @symbolKywd@ attribute symbol.
aliasKywdId :: Id
aliasKywdId = "alias'Kywd'"

-- | Kore symbol representing the @alias@ attribute.
aliasKywdSymbol :: SymbolOrAlias
aliasKywdSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = aliasKywdId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @symbolKywd@ attribute.
aliasKywdAttribute :: AttributePattern
aliasKywdAttribute = attributePattern_ aliasKywdSymbol

instance ParseAttributes AliasKywd where
    parseAttribute = parseBoolAttribute aliasKywdId

instance From AliasKywd Attributes where
    from = toBoolAttributes aliasKywdAttribute
