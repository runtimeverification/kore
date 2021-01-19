{-|
Module      : Kore.Attribute.Concat
Description : Concat attribute
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

-}
module Kore.Attribute.Concat
    ( Concat (..)
    , concatId, concatSymbol, concatAttribute
    ) where

import Prelude.Kore

import Data.Default
import qualified Generics.SOP as SOP

import Kore.Attribute.Parser
import Kore.Debug

-- | @Concat@ represents the @concat@ attribute.
newtype Concat = Concat { getConcat :: Maybe SymbolOrAlias }
    deriving (Generic, Eq, Ord, Show)
    deriving anyclass (Hashable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Semigroup Concat where
    (<>) a@(Concat (Just _)) _ = a
    (<>) _                     b = b

instance Monoid Concat where
    mempty = Concat { getConcat = Nothing }

instance Default Concat where
    def = mempty

instance NFData Concat

instance ParseAttributes Concat where
    parseAttribute = withApplication' parseApplication
      where
        parseApplication params args Concat { getConcat }
          | Just _ <- getConcat = failDuplicate'
          | otherwise = do
            getZeroParams params
            arg <- getOneArgument args
            symbol <- getSymbolOrAlias arg
            return Concat { getConcat = Just symbol }
        withApplication' = withApplication concatId
        failDuplicate' = failDuplicate concatId

instance From Concat Attributes where
    from =
        maybe def toAttribute . getConcat
      where
        toAttribute = from @AttributePattern . concatAttribute

-- | Kore identifier representing the @concat@ attribute symbol.
concatId :: Id
concatId = "concat"

-- | Kore symbol representing the @concat@ attribute.
concatSymbol :: SymbolOrAlias
concatSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = concatId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @concat@ attribute.
concatAttribute :: SymbolOrAlias -> AttributePattern
concatAttribute symbol =
    attributePattern concatSymbol [attributePattern_ symbol]
