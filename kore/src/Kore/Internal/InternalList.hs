{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Internal.InternalList (
    InternalList (..),
) where

import Data.Binary (Binary)
import Data.Sequence (
    Seq,
 )
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.ConstructorLike
import Kore.Attribute.Pattern.Defined
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
 )
import Kore.Attribute.Pattern.Function
import Kore.Attribute.Pattern.Simplified
import Kore.Attribute.Pattern.Total
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Sort
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

-- | Internal representation of the builtin @LIST.List@ domain.
data InternalList child = InternalList
    { internalListSort :: !Sort
    , internalListUnit :: !Symbol
    , internalListElement :: !Symbol
    , internalListConcat :: !Symbol
    , internalListChild :: !(Seq child)
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Foldable, Functor, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Hashable child => Hashable (InternalList child) where
    hashWithSalt salt internal =
        hashWithSalt salt (toList internalListChild)
      where
        InternalList{internalListChild} = internal

instance NFData child => NFData (InternalList child)

instance Unparse child => Unparse (InternalList child) where
    unparse internalList =
        Pretty.hsep
            [ "/* InternalList: */"
            , unparseConcat'
                (unparse internalListUnit)
                (unparse internalListConcat)
                (element <$> toList internalListChild)
            ]
      where
        element x = unparse internalListElement <> arguments [x]
        InternalList{internalListChild} = internalList
        InternalList{internalListUnit} = internalList
        InternalList{internalListElement} = internalList
        InternalList{internalListConcat} = internalList

    unparse2 internalList =
        Pretty.hsep
            [ "/* InternalList: */"
            , unparseConcat'
                (unparse internalListUnit)
                (unparse internalListConcat)
                (element <$> toList internalListChild)
            ]
      where
        element x = unparse2 internalListElement <> arguments2 [x]
        InternalList{internalListChild} = internalList
        InternalList{internalListUnit} = internalList
        InternalList{internalListElement} = internalList
        InternalList{internalListConcat} = internalList

instance Synthetic Sort InternalList where
    synthetic = internalListSort
    {-# INLINE synthetic #-}

instance Ord variable => Synthetic (FreeVariables variable) InternalList where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike InternalList where
    synthetic internalList
        | all isConstructorLike internalList =
            ConstructorLike (Just ConstructorLikeHead)
        | otherwise = ConstructorLike Nothing
    {-# INLINE synthetic #-}

instance Synthetic Defined InternalList where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Function InternalList where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Total InternalList where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Simplified InternalList where
    synthetic = notSimplified
    {-# INLINE synthetic #-}
