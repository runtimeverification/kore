{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause

Foreign keys for SQL tables.
-}
module SQL.Key (
    Key (..),
) where

import Data.Binary (Binary)
import Data.Int (
    Int64,
 )
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Prelude.Kore

-- | A foreign key into the table for type @a@.
newtype Key a = Key {getKey :: Int64}
    deriving stock (Eq, Ord, Read, Show)
    deriving stock (Functor, Foldable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
