{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Pattern.Defined (
    Defined (..),
    alwaysDefined,
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

-- | A pattern is 'Defined' if it matches at least one element.
newtype Defined = Defined {isDefined :: Bool}
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)
    deriving (Semigroup, Monoid) via All

alwaysDefined :: a -> Defined
alwaysDefined = const (Defined True)

instance Synthetic Defined (And sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Bottom sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

{- | An 'Application' pattern is 'Defined' if the symbol is total and its
 arguments are 'Defined'.
-}
instance Synthetic Defined (Application Internal.Symbol) where
    synthetic application =
        totalSymbol <> fold children
      where
        totalSymbol = Defined (Internal.isNotBottom symbol)
        children = applicationChildren application
        symbol = applicationSymbolOrAlias application

-- TODO: calculate by looking at RHS of alias
instance Synthetic Defined (Application (Internal.Alias patternType)) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Ceil sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

-- | A 'DomainValue' patterns is 'Defined' if its argument is 'Defined'.
instance Synthetic Defined (DomainValue sort) where
    synthetic = domainValueChild
    {-# INLINE synthetic #-}

instance Synthetic Defined (Equals sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Exists sort variable) where
    synthetic = const (Defined False)

instance Synthetic Defined (Floor sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Forall sort variable) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Iff sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Implies sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (In sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Mu sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

-- | A 'Next' pattern is 'Defined' if its argument is 'Defined'.
instance Synthetic Defined (Next sort) where
    synthetic = nextChild
    {-# INLINE synthetic #-}

instance Synthetic Defined (Not sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Nu sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

-- | An 'Or' pattern is 'Defined' if any of its subterms is 'Defined'.
instance Synthetic Defined (Or sort) where
    synthetic = Defined . getAny . foldMap (Any . isDefined)
    {-# INLINE synthetic #-}

instance Synthetic Defined (Rewrites sort) where
    synthetic = const (Defined False)
    {-# INLINE synthetic #-}

-- | A 'Top' pattern is always 'Defined'.
instance Synthetic Defined (Top sort) where
    synthetic = alwaysDefined
    {-# INLINE synthetic #-}

-- | A 'StringLiteral' pattern is always 'Defined'.
instance Synthetic Defined (Const StringLiteral) where
    synthetic = alwaysDefined
    {-# INLINE synthetic #-}

-- | An 'Inhabitant' pattern is always 'Defined'.
instance Synthetic Defined Inhabitant where
    synthetic = alwaysDefined
    {-# INLINE synthetic #-}

{- | An element variable pattern is always 'Defined'.
   A set variable is not.
-}
instance Synthetic Defined (Const (SomeVariable variable)) where
    synthetic (Const unifiedVariable) =
        Defined (isElementVariable unifiedVariable)
    {-# INLINE synthetic #-}

instance Synthetic Defined Inj where
    synthetic = synthetic . Inj.toApplication
