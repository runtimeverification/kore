{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Pattern.ConstructorLike (
    ConstructorLike (..),
    ConstructorLikeHead (..),
    HasConstructorLike (..),
    assertConstructorLike,
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.Alias (
    Alias,
 )
import Kore.Internal.Inj (
    Inj (..),
 )
import Kore.Internal.InternalBytes (
    InternalBytes,
 )
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Internal.Symbol qualified as Symbol
import Kore.Syntax
import Prelude.Kore

{- | A pattern is 'ConstructorLike' if logical equality is syntactic equality.

In other words, a pattern is constructor-like if it is equal (in the logical
'Equals' sense) to another constructor-like pattern if and only if it is
syntactically equal (in the 'Eq' sense).

Examples of patterns that are constructor-like:

* 'InternalBool', 'InternalInt', 'InternalString', and 'InternalBytes'
* 'StringLiteral'
* constructors with constructor-like arguments
* 'DomainValue' in a non-hooked sort
* 'Inj' in its normal form (if its argument is not also 'Inj')

Examples of patterns that are not constructor-like:

* variables
* function symbols
* logical connectives
-}
newtype ConstructorLike = ConstructorLike
    { getConstructorLike :: Maybe ConstructorLikeHead
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance Diff ConstructorLike where
    diffPrec = diffPrecIgnore

instance Synthetic ConstructorLike (And sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Bottom sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Application Symbol) where
    synthetic application
        -- The constructor application is constructor-like if all
        -- its children are constructor-like.
        | Symbol.isConstructor symbol
        , childrenAreConstructorLike =
            ConstructorLike . Just $ ConstructorLikeHead
        -- Checks that the children of a sort injection are
        -- not sort injections, i.e. that the triangle axiom
        -- for sort injections has been fully applied.
        | Symbol.isSortInjection symbol
        , childrenAreConstructorLike
        , childrenAreNotSortInjections =
            ConstructorLike . Just $ SortInjectionHead
        | otherwise =
            ConstructorLike Nothing
      where
        symbol = applicationSymbolOrAlias application
        children = applicationChildren application
        childrenAreConstructorLike =
            ConstructorLike Nothing `notElem` children
        childrenAreNotSortInjections =
            (ConstructorLike . Just $ SortInjectionHead) `notElem` children

instance Synthetic ConstructorLike (Application (Alias patternType)) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Ceil sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

{- |
A domain value is not technically a constructor, but it is constructor-like for
builtin domains, at least from the perspective of normalization (normalized
means constructor-like here).
-}
instance Synthetic ConstructorLike (DomainValue sort) where
    synthetic domainValue
        | isJust . getConstructorLike $ child =
            ConstructorLike . Just $ ConstructorLikeHead
        | otherwise =
            ConstructorLike Nothing
      where
        child = domainValueChild domainValue

instance Synthetic ConstructorLike (Equals sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Exists sort variable) where
    synthetic = const (ConstructorLike Nothing)

instance Synthetic ConstructorLike (Floor sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Forall sort variable) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Iff sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Implies sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (In sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Mu sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Next sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Not sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Nu sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Or sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Rewrites sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike Inhabitant where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const (SomeVariable variable)) where
    synthetic = const (ConstructorLike Nothing)

instance Synthetic ConstructorLike (Const StringLiteral) where
    synthetic = const (ConstructorLike . Just $ ConstructorLikeHead)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const InternalBytes) where
    synthetic = const (ConstructorLike . Just $ ConstructorLikeHead)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Top sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike Inj where
    synthetic Inj{injChild} = ConstructorLike $ do
        childHead <- getConstructorLike injChild
        case childHead of
            SortInjectionHead -> Nothing
            _ -> pure SortInjectionHead
    {-# INLINE synthetic #-}

data ConstructorLikeHead
    = ConstructorLikeHead
    | SortInjectionHead
    deriving stock (Eq, Ord, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance Diff ConstructorLikeHead where
    diffPrec = diffPrecIgnore

class HasConstructorLike a where
    extractConstructorLike :: a -> ConstructorLike

    isConstructorLike :: a -> Bool
    isConstructorLike a = case extractConstructorLike a of
        (ConstructorLike constructorLike) -> isJust constructorLike

instance HasConstructorLike ConstructorLike where
    extractConstructorLike = id

assertConstructorLike :: HasConstructorLike a => String -> a -> b -> b
assertConstructorLike message a =
    if not (isConstructorLike a)
        then error ("Expecting constructor-like object. " ++ message)
        else id
