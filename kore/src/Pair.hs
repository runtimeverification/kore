{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Pair (
    Pair (..),
) where

import Data.Binary (Binary)
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Prelude.Kore

-- | A tuple @(a, a)@ where both elements are the same type.
data Pair a = Pair !a !a
    deriving stock (Eq, Ord, Read, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving stock (Foldable, Functor, Traversable)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
