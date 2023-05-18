{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.In (
    In (..),
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

{- | 'In' corresponds to the @\\in@ branch of the @matching-logic-pattern@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

'inOperandSort' is the sort of the operands.

'inResultSort' is the sort of the result.
-}
data In sort child = In
    { inOperandSort :: !sort
    , inResultSort :: !sort
    , inContainedChild :: !child
    , inContainingChild :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (In Sort child) where
    unparse
        In
            { inOperandSort
            , inResultSort
            , inContainedChild
            , inContainingChild
            } =
            "\\in"
                <> parameters [inOperandSort, inResultSort]
                <> arguments [inContainedChild, inContainingChild]

    unparse2
        In
            { inContainedChild
            , inContainingChild
            } =
            Pretty.parens
                ( Pretty.fillSep
                    [ "\\in"
                    , unparse2 inContainedChild
                    , unparse2 inContainingChild
                    ]
                )

instance Unparse child => Unparse (In () child) where
    unparse
        In
            { inContainedChild
            , inContainingChild
            } =
            "\\in"
                <> arguments [inContainedChild, inContainingChild]

    unparse2
        In
            { inContainedChild
            , inContainingChild
            } =
            Pretty.parens
                ( Pretty.fillSep
                    [ "\\in"
                    , unparse2 inContainedChild
                    , unparse2 inContainingChild
                    ]
                )

instance Ord variable => Synthetic (FreeVariables variable) (In sort) where
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Sort (In Sort) where
    synthetic in' =
        inResultSort
            & seq (sameSort inOperandSort inContainedChild)
                . seq (sameSort inOperandSort inContainingChild)
      where
        In{inResultSort, inOperandSort} = in'
        In{inContainedChild, inContainingChild} = in'
    {-# INLINE synthetic #-}
