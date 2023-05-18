{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Pattern.Function (
    Function (..),
    alwaysFunction,
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

-- | A pattern is 'Function' if it matches zero or one elements.
newtype Function = Function {isFunction :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via All

alwaysFunction :: a -> Function
alwaysFunction = const (Function True)
{-# INLINE alwaysFunction #-}

instance Synthetic Function (And sort) where
    -- TODO (thomas.tuegel):
    -- synthetic = getAny . Foldable.foldMap (Any . isFunction)
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

-- | A 'Bottom' pattern is always 'Function'.
instance Synthetic Function (Bottom sort) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

{- | An 'Application' pattern is 'Function' if its symbol is a function and its
 arguments are 'Function'.
-}
instance Synthetic Function (Application Internal.Symbol) where
    synthetic application =
        functionSymbol <> fold children
      where
        functionSymbol = Function (Internal.isFunction symbol)
        children = applicationChildren application
        symbol = applicationSymbolOrAlias application

instance Synthetic Function (Application (Internal.Alias patternType)) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Ceil sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

-- | A 'DomainValue' pattern is 'Function' if its argument is 'Function'.
instance Synthetic Function (DomainValue sort) where
    synthetic = domainValueChild
    {-# INLINE synthetic #-}

instance Synthetic Function (Equals sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Exists sort variable) where
    synthetic = const (Function False)

instance Synthetic Function (Floor sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Forall sort variable) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Iff sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Implies sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (In sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Mu sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Next sort) where
    synthetic = nextChild
    {-# INLINE synthetic #-}

instance Synthetic Function (Not sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Nu sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Or sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Rewrites sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Top sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

-- | A 'StringLiteral' pattern is always 'Function'.
instance Synthetic Function (Const StringLiteral) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

-- | An 'Inhabitant' pattern is never 'Function'.
instance Synthetic Function Inhabitant where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

instance Synthetic Function (Const (SomeVariable variable)) where
    synthetic (Const unifiedVariable) =
        Function (isElementVariable unifiedVariable)
    {-# INLINE synthetic #-}

instance Synthetic Function Inj where
    synthetic = synthetic . Inj.toApplication
    {-# INLINE synthetic #-}
