{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Iff (
    Iff (..),
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

{- | 'Iff' corresponds to the @\\iff@ branch of the @matching-logic-pattern@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

'iffSort' is both the sort of the operands and the sort of the result.
-}
data Iff sort child = Iff
    { iffSort :: !sort
    , iffFirst :: !child
    , iffSecond :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (Iff Sort child) where
    unparse Iff{iffSort, iffFirst, iffSecond} =
        "\\iff"
            <> parameters [iffSort]
            <> arguments [iffFirst, iffSecond]

    unparse2 Iff{iffFirst, iffSecond} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\iff"
                , unparse2 iffFirst
                , unparse2 iffSecond
                ]
            )

instance Unparse child => Unparse (Iff () child) where
    unparse Iff{iffFirst, iffSecond} =
        "\\iff"
            <> arguments [iffFirst, iffSecond]

    unparse2 Iff{iffFirst, iffSecond} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\iff"
                , unparse2 iffFirst
                , unparse2 iffSecond
                ]
            )

instance Ord variable => Synthetic (FreeVariables variable) (Iff sort) where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Sort (Iff Sort) where
    synthetic Iff{iffSort, iffFirst, iffSecond} =
        iffSort
            & seq (sameSort iffSort iffFirst)
                . seq (sameSort iffSort iffSecond)
    {-# INLINE synthetic #-}
