{- |
Copyright   : (c) Runtime Verification, 2018-2023
License     : BSD-3-Clause
-}
module Kore.Attribute.Total (
    functionalId,
    Total (..),
    totalId,
    totalSymbol,
    totalAttribute,
) where

import Data.Binary (Binary)
import Data.Monoid qualified as Monoid
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | Kore identifier representing the @functional@ attribute symbol.
functionalId :: Id
functionalId = "functional"

newtype Total = Total {isDeclaredTotal :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via Monoid.Any

instance Default Total where
    def = mempty

totalId :: Id
totalId = "total"

totalSymbol :: SymbolOrAlias
totalSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = totalId
        , symbolOrAliasParams = []
        }

totalAttribute :: AttributePattern
totalAttribute = attributePattern_ totalSymbol

instance ParseAttributes Total where
    parseAttribute pat att =
        {- TODO: When we don't need anymore to support parsing @functional@ attribute as
           @total@, the function should look as follows:

            parseAttribute = parseBoolAttribute totalId

           Don't forget to remove `functionalId` definition above and uncomment
           `test_duplicate` in Test.Kore.Attribute.Total
        -}
        if (isDeclaredTotal att)
            then return att
            else do
                totalAttr <- parseBoolAttribute totalId pat att
                totalAttr' <- parseBoolAttribute functionalId pat att
                pure $ Total (isDeclaredTotal totalAttr || isDeclaredTotal totalAttr')

instance From Total Attributes where
    from = toBoolAttributes totalAttribute
