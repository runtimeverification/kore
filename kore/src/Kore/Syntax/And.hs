{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.And (
    And (..),
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

{- | 'And' corresponds to the @\\and@ branch of the @matching-logic-pattern@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

'andSort' is both the sort of the operands and the sort of the result.

This represents the 'andFirst ∧ andSecond' Matching Logic construct.
-}
data And sort child = And
    { andSort :: !sort
    , andFirst :: !child
    , andSecond :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (And Sort child) where
    unparse And{andSort, andFirst, andSecond} =
        "\\and"
            <> parameters [andSort]
            <> arguments [andFirst, andSecond]

    unparse2 And{andFirst, andSecond} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\and"
                , unparse2 andFirst
                , unparse2 andSecond
                ]
            )

instance Ord variable => Synthetic (FreeVariables variable) (And sort) where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Sort (And Sort) where
    synthetic And{andSort, andFirst, andSecond} =
        andSort
            & seq (sameSort andSort andFirst)
                . seq (sameSort andSort andSecond)
    {-# INLINE synthetic #-}
