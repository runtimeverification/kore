{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.StringLiteral (
    StringLiteral (..),
) where

import Data.Binary (Binary)
import Data.Functor.Const
import Data.Text (
    Text,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
    emptyFreeVariables,
 )
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

-- | 'StringLiteral' corresponds to the @string-literal@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#string-literals kore-syntax.md#string-literals>.
newtype StringLiteral = StringLiteral {getStringLiteral :: Text}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse StringLiteral where
    unparse = Pretty.dquotes . Pretty.pretty . escapeStringT . getStringLiteral
    unparse2 = unparse

instance Synthetic (FreeVariables variable) (Const StringLiteral) where
    synthetic = const emptyFreeVariables
    {-# INLINE synthetic #-}

instance Synthetic Sort (Const StringLiteral) where
    synthetic = const stringMetaSort
    {-# INLINE synthetic #-}
