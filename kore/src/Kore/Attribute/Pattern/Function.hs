{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.Attribute.Pattern.Function
    ( Function (..)
    ) where

import Prelude.Kore

import Control.DeepSeq
import qualified Data.Foldable as Foldable
import Data.Functor.Const
import Data.Monoid
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Domain.Builtin
import qualified Kore.Internal.Alias as Internal
import Kore.Internal.Inj
    ( Inj
    )
import qualified Kore.Internal.Inj as Inj
import Kore.Internal.InternalBytes
    ( InternalBytes
    )
import qualified Kore.Internal.Symbol as Internal
import Kore.Syntax
import Kore.Variables.UnifiedVariable
    ( isElementVariable
    )

{- | A pattern is 'Function' if it matches zero or one elements.
 -}
newtype Function = Function { isFunction :: Bool }
    deriving (Eq, GHC.Generic, Show)
    deriving (Semigroup, Monoid) via All

instance SOP.Generic Function

instance SOP.HasDatatypeInfo Function

instance Debug Function

instance Diff Function

instance NFData Function

instance Hashable Function

instance Synthetic Function (And sort) where
    -- TODO (thomas.tuegel):
    -- synthetic = getAny . Foldable.foldMap (Any . isFunction)
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

-- | A 'Bottom' pattern is always 'Function'.
instance Synthetic Function (Bottom sort) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

-- | An 'Application' pattern is 'Function' if its symbol is a function and its
-- arguments are 'Function'.
instance Synthetic Function (Application Internal.Symbol) where
    synthetic application =
        functionSymbol <> Foldable.fold children
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

-- | A 'Builtin' pattern is 'Function' if its subterms are 'Function'.
instance Synthetic Function (Builtin key) where
    synthetic = Foldable.fold
    {-# INLINE synthetic #-}

instance Synthetic Function (Top sort) where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

-- | A 'StringLiteral' pattern is always 'Function'.
instance Synthetic Function (Const StringLiteral) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

-- | A 'Bytes' pattern is always 'Function'.
instance Synthetic Function (Const InternalBytes) where
    synthetic = const (Function True)
    {-# INLINE synthetic #-}

-- | An 'Inhabitant' pattern is never 'Function'.
instance Synthetic Function Inhabitant where
    synthetic = const (Function False)
    {-# INLINE synthetic #-}

-- | A 'Variable' pattern is always 'Function'.
instance Synthetic Function (Const (SomeVariable variable)) where
    synthetic (Const unifiedVariable) = Function (isElementVariable unifiedVariable)
    {-# INLINE synthetic #-}

instance Synthetic Function Inj where
    synthetic = synthetic . Inj.toApplication
    {-# INLINE synthetic #-}
