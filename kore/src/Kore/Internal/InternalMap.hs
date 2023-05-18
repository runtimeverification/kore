{- |
Copyright : (c) Runtime Verification, 2021
License   : BSD-3-Clause
-}
module Kore.Internal.InternalMap (
    InternalMap,
    InternalAc (..),
    Element (..),
    MapElement,
    Value (..),
    MapValue,
    NormalizedMap (..),

    -- * Re-exports
    module Kore.Internal.NormalizedAc,
) where

import Control.Lens qualified as Lens
import Data.Bifunctor qualified as Bifunctor
import Data.Binary (Binary)
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
import Kore.Internal.NormalizedAc
import Kore.Sort
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

-- * Builtin Map

-- | Wrapper for map values.
type MapValue = Value NormalizedMap

-- | Wrapper for map elements.
type MapElement = Element NormalizedMap

-- | Wrapper for normalized maps, to be used in the `builtinAcChild` field.
newtype NormalizedMap key child = NormalizedMap {getNormalizedMap :: NormalizedAc NormalizedMap key child}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving newtype (Foldable, Functor)
    deriving stock (Traversable)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance AcWrapper NormalizedMap where
    newtype Value NormalizedMap child = MapValue {getMapValue :: child}
        deriving stock (Eq, Ord, Show)
        deriving stock (Foldable, Functor, Traversable)
        deriving stock (GHC.Generic)
        deriving anyclass (Binary)
        deriving anyclass (Hashable, NFData)
        deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
        deriving anyclass (Debug, Diff)

    newtype Element NormalizedMap child = MapElement {getMapElement :: (child, child)}
        deriving stock (Eq, Ord, Show)
        deriving stock (Foldable, Functor, Traversable)
        deriving stock (GHC.Generic)
        deriving anyclass (Binary)
        deriving anyclass (Hashable, NFData)
        deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
        deriving anyclass (Debug, Diff)

    wrapAc = NormalizedMap
    unwrapAc = getNormalizedMap
    alignValues (MapValue a) (MapValue b) = MapValue (a, b)

    elementIso =
        Lens.iso
            (Bifunctor.second MapValue . getMapElement)
            (MapElement . Bifunctor.second getMapValue)

    unparseElement childUnparser (MapElement (key, value)) =
        arguments' [childUnparser key, childUnparser value]
    unparseConcreteElement keyUnparser childUnparser (key, MapValue value) =
        arguments' [keyUnparser key, childUnparser value]

    elementToApplicationArgs (MapElement (key, value)) = [key, value]
    concreteElementToApplicationArgs (MapValue value) = [value]

-- | Internal representation of the builtin @MAP.Map@ domain.
type InternalMap key = InternalAc key NormalizedMap

instance (Unparse key, Unparse child) => Unparse (InternalMap key child) where
    unparse internalMap =
        Pretty.hsep
            [ "/* InternalMap: */"
            , unparseInternalAc unparse unparse internalMap
            ]
    unparse2 internalMap =
        Pretty.hsep
            [ "/* InternalMap: */"
            , unparseInternalAc unparse2 unparse2 internalMap
            ]

-- | A 'Builtin' pattern is defined if its subterms are 'Defined'.
instance Synthetic Defined (InternalMap key) where
    synthetic InternalAc{builtinAcChild = NormalizedMap builtinMapChild} =
        normalizedAcDefined builtinMapChild
    {-# INLINE synthetic #-}

instance
    Ord variable =>
    Synthetic (FreeVariables variable) (InternalMap key)
    where
    synthetic = fold
    {-# INLINE synthetic #-}

-- | A 'Builtin' pattern is 'Function' if its subterms are 'Function'.
instance Synthetic Function (InternalMap key) where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Total (InternalMap key) where
    synthetic InternalAc{builtinAcChild = NormalizedMap builtinMapChild} =
        normalizedAcTotal builtinMapChild
    {-# INLINE synthetic #-}

instance Synthetic Sort (InternalMap key) where
    synthetic = builtinAcSort
    {-# INLINE synthetic #-}

instance Synthetic Simplified (InternalMap key) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance HasConstructorLike (Value NormalizedMap ConstructorLike) where
    extractConstructorLike (MapValue result) = result

instance
    HasConstructorLike key =>
    Synthetic ConstructorLike (InternalMap key)
    where
    synthetic InternalAc{builtinAcChild = NormalizedMap builtinMapChild} =
        normalizedAcConstructorLike builtinMapChild
