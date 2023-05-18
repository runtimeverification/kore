{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.Next (
    Next (..),
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

{- | 'Next' corresponds to the @\\next@ branch of the @matching-logic-pattern@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

'nextSort' is both the sort of the operand and the sort of the result.
-}
data Next sort child = Next
    { nextSort :: !sort
    , nextChild :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (Next Sort child) where
    unparse Next{nextSort, nextChild} =
        "\\next"
            <> parameters [nextSort]
            <> arguments [nextChild]

    unparse2 Next{nextChild} =
        Pretty.parens (Pretty.fillSep ["\\next", unparse2 nextChild])

instance Synthetic (FreeVariables variable) (Next sort) where
    synthetic = nextChild
    {-# INLINE synthetic #-}

instance Synthetic Sort (Next Sort) where
    synthetic Next{nextSort, nextChild} =
        nextSort `sameSort` nextChild
    {-# INLINE synthetic #-}
