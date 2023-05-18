{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Label (
    Label (..),
    labelId,
    labelSymbol,
    labelAttribute,
) where

import Data.Binary (Binary)
import Data.Monoid qualified as Monoid
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore

-- | @Label@ represents the @overload@ attribute for symbols.
newtype Label = Label {unLabel :: Maybe Text}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via (Monoid.First Text)

instance Default Label where
    def = mempty

-- | Kore identifier representing the @label@ attribute symbol.
labelId :: Id
labelId = "label"

-- | Kore symbol representing the @label@ attribute.
labelSymbol :: SymbolOrAlias
labelSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = labelId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @overload@ attribute.
labelAttribute :: Text -> AttributePattern
labelAttribute label = attributePattern labelSymbol [attributeString label]

instance ParseAttributes Label where
    parseAttribute = withApplication' parseApplication
      where
        parseApplication params args Label{unLabel}
            | Just _ <- unLabel = failDuplicate'
            | otherwise = do
                Parser.getZeroParams params
                arg1 <- Parser.getOneArgument args
                StringLiteral str <- Parser.getStringLiteral arg1
                return Label{unLabel = Just str}
        withApplication' = Parser.withApplication labelId
        failDuplicate' = Parser.failDuplicate labelId

instance From Label Attributes where
    from = maybe def (from @AttributePattern . labelAttribute) . unLabel
