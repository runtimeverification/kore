{- |
Module      : Kore.Attribute.Sort.Concat
Description : Concat sort attribute
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Sort.Concat (
    Concat (..),
    concatId,
    concatSymbol,
    concatAttribute,
) where

import Data.Binary (Binary)
import Data.Default
import Kore.Attribute.Parser
import Prelude.Kore

-- | @Concat@ represents the @concat@ attribute for sorts.
newtype Concat = Concat {getConcat :: Maybe SymbolOrAlias}
    deriving stock (Generic, Eq, Ord, Show)
    deriving anyclass (Binary)

instance Semigroup Concat where
    (<>) a@(Concat (Just _)) _ = a
    (<>) _ b = b

instance Monoid Concat where
    mempty = Concat{getConcat = Nothing}

instance Default Concat where
    def = mempty

instance NFData Concat

instance ParseAttributes Concat where
    parseAttribute = withApplication' parseApplication
      where
        parseApplication params args Concat{getConcat}
            | Just _ <- getConcat = failDuplicate'
            | otherwise = do
                getZeroParams params
                arg <- getOneArgument args
                symbol <- getSymbolOrAlias arg
                return Concat{getConcat = Just symbol}
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
