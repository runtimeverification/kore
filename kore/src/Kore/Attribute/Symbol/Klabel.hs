{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Symbol.Klabel (
    Klabel (..),
    klabelId,
    klabelSymbol,
    klabelAttribute,
) where

import Data.Binary (Binary)
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Parser as Parser
import Kore.Debug
import Prelude.Kore
import Pretty

-- | @Klabel@ represents the @klabel@ attribute for symbols.
newtype Klabel = Klabel {getKlabel :: Maybe Text}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Klabel where
    def = Klabel Nothing

instance Pretty Klabel where
    pretty (Klabel (Just text)) = pretty text
    pretty (Klabel Nothing) = "<no label>"

-- | Kore identifier representing the @klabel@ attribute symbol.
klabelId :: Id
klabelId = "klabel"

-- | Kore symbol representing the @klabel@ attribute.
klabelSymbol :: SymbolOrAlias
klabelSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = klabelId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @klabel@ attribute.
klabelAttribute :: Text -> AttributePattern
klabelAttribute name =
    attributePattern klabelSymbol [attributeString name]

instance ParseAttributes Klabel where
    parseAttribute = withApplication' parseApplication
      where
        parseApplication params args Klabel{getKlabel} = do
            Parser.getZeroParams params
            arg <- Parser.getOneArgument args
            StringLiteral name <- Parser.getStringLiteral arg
            unless (isNothing getKlabel) failDuplicate'
            return Klabel{getKlabel = Just name}
        withApplication' = Parser.withApplication klabelId
        failDuplicate' = Parser.failDuplicate klabelId

instance From Klabel Attributes where
    from =
        maybe def toAttribute . getKlabel
      where
        toAttribute = from @AttributePattern . klabelAttribute
