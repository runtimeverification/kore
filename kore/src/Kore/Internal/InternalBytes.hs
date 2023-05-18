{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Internal.InternalBytes (
    InternalBytes (..),
) where

import Data.Binary (Binary)
import Data.ByteString.Short (
    ShortByteString,
 )
import Data.ByteString.Short qualified as ByteString
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.Defined
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Pattern.Function
import Kore.Attribute.Pattern.Total
import Kore.Attribute.Synthetic
import Kore.Builtin.Encoding qualified as Encoding
import Kore.Debug
import Kore.Syntax
import Kore.Unparser
import Prelude.Kore

data InternalBytes = InternalBytes
    { internalBytesSort :: !Sort
    , internalBytesValue :: !ShortByteString
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse InternalBytes where
    unparse internalBytes@(InternalBytes _ _) =
        "\\dv"
            <> parameters [internalBytesSort]
            <> arguments [StringLiteral (Encoding.decode8Bit $ ByteString.fromShort internalBytesValue)]
      where
        InternalBytes{internalBytesSort, internalBytesValue} = internalBytes

    unparse2 internalBytes@(InternalBytes _ _) =
        "\\dv2"
            <> parameters2 [internalBytesSort]
            <> arguments2 [StringLiteral (Encoding.decode8Bit $ ByteString.fromShort internalBytesValue)]
      where
        InternalBytes{internalBytesSort, internalBytesValue} = internalBytes

instance Synthetic Sort (Const InternalBytes) where
    synthetic = internalBytesSort . getConst
    {-# INLINE synthetic #-}

instance Synthetic (FreeVariables variable) (Const InternalBytes) where
    synthetic = const emptyFreeVariables
    {-# INLINE synthetic #-}

instance Synthetic Defined (Const InternalBytes) where
    synthetic = alwaysDefined
    {-# INLINE synthetic #-}

instance Synthetic Function (Const InternalBytes) where
    synthetic = alwaysFunction
    {-# INLINE synthetic #-}

instance Synthetic Total (Const InternalBytes) where
    synthetic = alwaysTotal
    {-# INLINE synthetic #-}
