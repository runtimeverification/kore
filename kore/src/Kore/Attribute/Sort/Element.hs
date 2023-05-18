{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Sort.Element (
    Element (..),
    elementId,
    elementSymbol,
    elementAttribute,
) where

import Data.Binary (Binary)
import Data.Default
import Kore.Attribute.Parser
import Prelude.Kore

-- | @Element@ represents the @element@ attribute for sorts.
newtype Element = Element {getElement :: Maybe SymbolOrAlias}
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass (Binary)

instance Semigroup Element where
    (<>) a@(Element (Just _)) _ = a
    (<>) _ b = b

instance Monoid Element where
    mempty = Element{getElement = Nothing}

instance Default Element where
    def = mempty

instance NFData Element

-- | Kore identifier representing the @element@ attribute symbol.
elementId :: Id
elementId = "element"

-- | Kore symbol representing the @element@ attribute.
elementSymbol :: SymbolOrAlias
elementSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = elementId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @element@ attribute.
elementAttribute :: SymbolOrAlias -> AttributePattern
elementAttribute symbol =
    attributePattern elementSymbol [attributePattern_ symbol]

instance ParseAttributes Element where
    parseAttribute = withApplication' parseApplication
      where
        parseApplication params args Element{getElement}
            | Just _ <- getElement = failDuplicate'
            | otherwise = do
                getZeroParams params
                arg <- getOneArgument args
                symbol <- getSymbolOrAlias arg
                return Element{getElement = Just symbol}
        withApplication' = withApplication elementId
        failDuplicate' = failDuplicate elementId

instance From Element Attributes where
    from =
        maybe def toAttribute . getElement
      where
        toAttribute = from @AttributePattern . elementAttribute
