{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Internal.InternalBool (
    InternalBool (..),
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

-- | Internal representation of the builtin @BOOL.Bool@ domain.
data InternalBool = InternalBool
    { internalBoolSort :: !Sort
    , internalBoolValue :: !Bool
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse InternalBool where
    unparse InternalBool{internalBoolSort, internalBoolValue} =
        "\\dv"
            <> parameters [internalBoolSort]
            <> Pretty.parens (Pretty.dquotes value)
      where
        value
            | internalBoolValue = "true"
            | otherwise = "false"

    unparse2 InternalBool{internalBoolSort, internalBoolValue} =
        "\\dv2"
            <> parameters2 [internalBoolSort]
            <> arguments' [Pretty.dquotes value]
      where
        value
            | internalBoolValue = "true"
            | otherwise = "false"

instance Synthetic Sort (Const InternalBool) where
    synthetic (Const InternalBool{internalBoolSort}) = internalBoolSort
    {-# INLINE synthetic #-}

instance Synthetic (FreeVariables variable) (Const InternalBool) where
    synthetic _ = emptyFreeVariables
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const InternalBool) where
    synthetic _ = ConstructorLike . Just $ ConstructorLikeHead
    {-# INLINE synthetic #-}

instance Synthetic Defined (Const InternalBool) where
    synthetic = alwaysDefined
    {-# INLINE synthetic #-}

instance Synthetic Function (Const InternalBool) where
    synthetic = alwaysFunction
    {-# INLINE synthetic #-}

instance Synthetic Total (Const InternalBool) where
    synthetic = alwaysTotal
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const InternalBool) where
    synthetic = alwaysSimplified
    {-# INLINE synthetic #-}
