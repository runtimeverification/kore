{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Implies (
    Implies (..),
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

{- | 'Implies' corresponds to the @\\implies@ branch of the @matching-logic-pattern@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

'impliesSort' is both the sort of the operands and the sort of the result.
-}
data Implies sort child = Implies
    { impliesSort :: !sort
    , impliesFirst :: !child
    , impliesSecond :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (Implies Sort child) where
    unparse Implies{impliesSort, impliesFirst, impliesSecond} =
        "\\implies"
            <> parameters [impliesSort]
            <> arguments [impliesFirst, impliesSecond]

    unparse2 Implies{impliesFirst, impliesSecond} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\implies"
                , unparse2 impliesFirst
                , unparse2 impliesSecond
                ]
            )

instance Unparse child => Unparse (Implies () child) where
    unparse Implies{impliesFirst, impliesSecond} =
        "\\implies"
            <> arguments [impliesFirst, impliesSecond]

    unparse2 Implies{impliesFirst, impliesSecond} =
        Pretty.parens
            ( Pretty.fillSep
                [ "\\implies"
                , unparse2 impliesFirst
                , unparse2 impliesSecond
                ]
            )

instance Ord variable => Synthetic (FreeVariables variable) (Implies sort) where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Sort (Implies Sort) where
    synthetic Implies{impliesSort, impliesFirst, impliesSecond} =
        impliesSort
            & seq (sameSort impliesSort impliesFirst)
                . seq (sameSort impliesSort impliesSecond)
    {-# INLINE synthetic #-}
