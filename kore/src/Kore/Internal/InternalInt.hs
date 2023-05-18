{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Internal.InternalInt (
    InternalInt (..),
) where

import Data.Binary (Binary)
import Data.Functor.Const
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.ConstructorLike
import Kore.Attribute.Pattern.Defined
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Pattern.Function
import Kore.Attribute.Pattern.Simplified
import Kore.Attribute.Pattern.Total
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

-- | Internal representation of the builtin @INT.Int@ domain.
data InternalInt = InternalInt {internalIntSort :: !Sort, internalIntValue :: !Integer}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse InternalInt where
    unparse InternalInt{internalIntSort, internalIntValue} =
        "\\dv"
            <> parameters [internalIntSort]
            <> Pretty.parens (Pretty.dquotes $ Pretty.pretty internalIntValue)

    unparse2 InternalInt{internalIntSort, internalIntValue} =
        "\\dv2"
            <> parameters2 [internalIntSort]
            <> arguments' [Pretty.dquotes $ Pretty.pretty internalIntValue]

instance Synthetic Sort (Const InternalInt) where
    synthetic (Const InternalInt{internalIntSort}) = internalIntSort

instance Synthetic (FreeVariables variable) (Const InternalInt) where
    synthetic _ = emptyFreeVariables

instance Synthetic ConstructorLike (Const InternalInt) where
    synthetic = const (ConstructorLike . Just $ ConstructorLikeHead)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Const InternalInt) where
    synthetic = alwaysDefined
    {-# INLINE synthetic #-}

instance Synthetic Function (Const InternalInt) where
    synthetic = alwaysFunction
    {-# INLINE synthetic #-}

instance Synthetic Total (Const InternalInt) where
    synthetic = alwaysTotal
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const InternalInt) where
    synthetic = alwaysSimplified
    {-# INLINE synthetic #-}
