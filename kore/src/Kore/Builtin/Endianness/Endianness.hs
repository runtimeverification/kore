{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Builtin.Endianness.Endianness (
    Endianness (..),
    toApplication,
) where

import Data.Binary (Binary)
import Data.Functor.Const
import Data.Void (
    Void,
 )
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.AST.AstWithLocation
import Kore.Attribute.Pattern.ConstructorLike
import Kore.Attribute.Pattern.Defined
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Pattern.Function
import Kore.Attribute.Pattern.Simplified
import Kore.Attribute.Pattern.Total
import Kore.Attribute.Synthetic
import Kore.Internal.Symbol
import Kore.Sort
import Kore.Syntax.Application (
    Application (..),
 )
import Kore.Unparser
import Prelude.Kore

data Endianness
    = BigEndian !Symbol
    | LittleEndian !Symbol
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)

instance Hashable Endianness

instance NFData Endianness

instance SOP.Generic Endianness

instance SOP.HasDatatypeInfo Endianness

instance Debug Endianness

instance Diff Endianness

instance Unparse Endianness where
    unparse = unparse . toApplication @Void
    unparse2 = unparse2 . toApplication @Void

instance Synthetic (FreeVariables variable) (Const Endianness) where
    synthetic = const emptyFreeVariables
    {-# INLINE synthetic #-}

instance Synthetic Sort (Const Endianness) where
    synthetic = synthetic . toApplication . getConst
    {-# INLINE synthetic #-}

instance Synthetic Total (Const Endianness) where
    synthetic = const (Total True)
    {-# INLINE synthetic #-}

instance Synthetic Function (Const Endianness) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Const Endianness) where
    synthetic = const (Defined True)
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const Endianness) where
    synthetic = const fullySimplified
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const Endianness) where
    synthetic =
        -- Endianness symbols are constructors
        const (ConstructorLike (Just ConstructorLikeHead))
    {-# INLINE synthetic #-}

instance AstWithLocation Endianness where
    locationFromAst (BigEndian symbol) = locationFromAst symbol
    locationFromAst (LittleEndian symbol) = locationFromAst symbol

toSymbol :: Endianness -> Symbol
toSymbol (BigEndian symbol) = symbol
toSymbol (LittleEndian symbol) = symbol

toApplication :: forall child. Endianness -> Application Symbol child
toApplication endianness = Application (toSymbol endianness) []
