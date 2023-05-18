{- |
Module      : Kore.Attribute.Null
Description : Null attribute parser
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
The 'Null' attribute is used when we need a type to satisfy the attribute
parser, but we do not actually care to parse any attributes. This parser simply
ignores all attributes.
This module is intended to be imported qualified:
@
import Kore.Attribute.Null qualified as Attribute
@
-}
module Kore.Attribute.Null (
    Null (..),
) where

import Data.Binary (Binary)
import Data.Default
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Prelude.Kore

data Null = Null
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Default Null where
    def = Null

instance Semigroup Null where
    (<>) _ _ = Null

instance Monoid Null where
    mempty = Null
