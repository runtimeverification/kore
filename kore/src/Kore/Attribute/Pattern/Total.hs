{- |
Copyright   : (c) Runtime Verification, 2019-2023
License     : BSD-3-Clause
-}
module Kore.Attribute.Pattern.Total (
    Total (..),
    alwaysTotal,
) where

import Data.Binary (Binary)
import Data.Functor.Const
import Data.Monoid
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.Alias qualified as Internal
import Kore.Internal.Inj (
    Inj,
 )
import Kore.Internal.Inj qualified as Inj
import Kore.Internal.Symbol qualified as Internal
import Kore.Syntax
import Prelude.Kore

-- | A pattern is 'Total' if it matches exactly one element.
newtype Total = Total {isTotal :: Bool}
    deriving stock (Eq, GHC.Generic, Ord, Show)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via All

alwaysTotal :: a -> Total
alwaysTotal = const (Total True)
{-# INLINE alwaysTotal #-}

instance Synthetic Total (And sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Bottom sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

-- | An 'Application' pattern is 'Total' if its symbol and its arguments are 'Total'.
instance Synthetic Total (Application Internal.Symbol) where
    synthetic application =
        totalSymbol <> fold children
      where
        totalSymbol = Total (Internal.isNotBottom symbol)
        children = applicationChildren application
        symbol = applicationSymbolOrAlias application

instance Synthetic Total (Application (Internal.Alias patternType)) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Ceil sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

-- | A 'DomainValue' pattern is 'Total' if its argument is 'Total'.
instance Synthetic Total (DomainValue sort) where
    synthetic = domainValueChild
    {-# INLINE synthetic #-}

instance Synthetic Total (Equals sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Exists sort variable) where
    synthetic = const (Total False)

instance Synthetic Total (Floor sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Forall sort variable) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Iff sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Implies sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (In sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Mu sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Next sort) where
    synthetic = nextChild
    {-# INLINE synthetic #-}

instance Synthetic Total (Not sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Nu sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Or sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Rewrites sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Top sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Const (SomeVariable variable)) where
    synthetic (Const unifiedVariable) =
        Total (isElementVariable unifiedVariable)
    {-# INLINE synthetic #-}

-- | A 'StringLiteral' pattern is always 'Total'.
instance Synthetic Total (Const StringLiteral) where
    synthetic = const (Total True)
    {-# INLINE synthetic #-}

-- | An 'Inhabitant' pattern is never 'Total'.
instance Synthetic Total Inhabitant where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total (Const Sort) where
    synthetic = const (Total False)
    {-# INLINE synthetic #-}

instance Synthetic Total Inj where
    synthetic = synthetic . Inj.toApplication
    {-# INLINE synthetic #-}
