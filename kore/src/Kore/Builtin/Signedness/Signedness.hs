{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Builtin.Signedness.Signedness (
    Signedness (..),
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

data Signedness
    = Signed !Symbol
    | Unsigned !Symbol
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse Signedness where
    unparse = unparse . toApplication @Void
    unparse2 = unparse2 . toApplication @Void

instance Synthetic (FreeVariables variable) (Const Signedness) where
    synthetic = const emptyFreeVariables
    {-# INLINE synthetic #-}

instance Synthetic Sort (Const Signedness) where
    synthetic = synthetic . toApplication . getConst
    {-# INLINE synthetic #-}

instance Synthetic Total (Const Signedness) where
    synthetic = const (Total True)
    {-# INLINE synthetic #-}

instance Synthetic Function (Const Signedness) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Const Signedness) where
    synthetic = const (Defined True)
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const Signedness) where
    synthetic = const fullySimplified
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const Signedness) where
    synthetic =
        -- Signedness symbols are constructors
        const (ConstructorLike (Just ConstructorLikeHead))
    {-# INLINE synthetic #-}

instance AstWithLocation Signedness where
    locationFromAst (Signed symbol) = locationFromAst symbol
    locationFromAst (Unsigned symbol) = locationFromAst symbol

toSymbol :: Signedness -> Symbol
toSymbol (Signed symbol) = symbol
toSymbol (Unsigned symbol) = symbol

toApplication :: forall child. Signedness -> Application Symbol child
toApplication signedness = Application (toSymbol signedness) []
