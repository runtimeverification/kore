-- The Synthetic KeyAttributes base instance needs this.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
-}
module Kore.Internal.Key (
    Key (..),
    KeyF (..),
    KeyAttributes (..),
) where

import Data.Binary (Binary (..))
import Data.Functor.Const (
    Const (..),
 )
import Data.Functor.Foldable (
    Base,
    Corecursive,
    Recursive,
 )
import Data.Functor.Foldable qualified as Recursive
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.ConstructorLike
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.Inj (
    Inj,
 )
import Kore.Internal.InternalBool
import Kore.Internal.InternalBytes
import Kore.Internal.InternalInt
import Kore.Internal.InternalList
import Kore.Internal.InternalMap
import Kore.Internal.InternalSet
import Kore.Internal.InternalString
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Sort (
    Sort,
 )
import Kore.Syntax.Application (
    Application (..),
 )
import Kore.Syntax.DomainValue (
    DomainValue (..),
 )
import Kore.Syntax.StringLiteral
import Kore.Unparser
import Prelude.Kore

{- | A type for 'Key' attributes. We only need to keep track of the sort.
 Keys are constructor-like, and therefore they are always
 fully simplified, defined, and total function-like ML patterns,
 which do not contain variables.
-}
newtype KeyAttributes = KeyAttributes
    { keySort :: Sort
    }
    deriving stock (Eq, Show)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- GHC claims the Functor constraint is redundant, but it gives an error
-- without it. See https://gitlab.haskell.org/ghc/ghc/-/issues/22838
instance (Synthetic Sort base, Functor base) => Synthetic KeyAttributes base where
    synthetic base =
        KeyAttributes
            { keySort = synthetic (keySort <$> base)
            }

instance HasConstructorLike KeyAttributes where
    extractConstructorLike _ =
        ConstructorLike . Just $ ConstructorLikeHead
    {-# INLINE extractConstructorLike #-}

-- | @Key@ is the type of patterns that may be concrete keys of maps and sets.
newtype Key = Key {getKey :: CofreeF KeyF KeyAttributes Key}
    deriving stock (Show)
    deriving stock (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance Binary Key where
    put (Recursive.project -> attrs :< keyF) =
        put attrs >> put keyF
    get = fmap Recursive.embed $ (:<) <$> get <*> get

type instance Base Key = CofreeF KeyF KeyAttributes

-- This instance implements all class functions for the TermLike newtype
-- because the their implementations for the inner type may be specialized.
instance Recursive Key where
    project = getKey
    {-# INLINE project #-}

-- This instance implements all class functions for the TermLike newtype
-- because the their implementations for the inner type may be specialized.
instance Corecursive Key where
    embed = Key
    {-# INLINE embed #-}

instance Diff Key where
    diffPrec
        key1@(Recursive.project -> attrs1 :< keyF1)
        key2@(Recursive.project -> _ :< keyF2) =
            -- If the patterns differ, do not display the difference in the
            -- attributes, which would overload the user with redundant information.
            diffPrecGeneric
                (Recursive.embed (attrs1 :< keyF1))
                (Recursive.embed (attrs1 :< keyF2))
                <|> diffPrecGeneric key1 key2

-- | This instance ignores the difference in attributes.
instance Eq Key where
    (==) (Recursive.project -> _ :< keyF1) (Recursive.project -> _ :< keyF2) =
        keyF1 == keyF2

-- | This instance ignores the difference in attributes.
instance Ord Key where
    compare
        (Recursive.project -> _ :< pat1)
        (Recursive.project -> _ :< pat2) =
            compare pat1 pat2

-- | This instance ignores the difference in attributes.
instance Hashable Key where
    hashWithSalt salt (Recursive.project -> _ :< pat) = hashWithSalt salt pat
    {-# INLINE hashWithSalt #-}

instance NFData Key where
    rnf (Recursive.project -> annotation :< pat) =
        rnf annotation `seq` rnf pat

instance Unparse Key where
    unparse (Recursive.project -> _ :< keyF) = unparse keyF
    {-# INLINE unparse #-}

    unparse2 (Recursive.project -> _ :< keyF) = unparse2 keyF
    {-# INLINE unparse2 #-}

instance HasConstructorLike Key where
    extractConstructorLike (Recursive.project -> attrs :< _) =
        extractConstructorLike attrs
    {-# INLINE extractConstructorLike #-}

instance From InternalInt Key where
    from = synthesize . from
    {-# INLINE from #-}

instance From InternalBool Key where
    from = synthesize . from
    {-# INLINE from #-}

instance From InternalString Key where
    from = synthesize . from
    {-# INLINE from #-}

-- | The base functor of 'Key'; the branching structure of the syntax tree.
data KeyF child
    = ApplySymbolF !(Application Symbol child)
    | InjF !(Inj child)
    | DomainValueF !(DomainValue Sort child)
    | InternalBoolF !(Const InternalBool child)
    | InternalBytesF !(Const InternalBytes child)
    | InternalIntF !(Const InternalInt child)
    | InternalListF !(InternalList child)
    | InternalMapF !(InternalMap Key child)
    | InternalSetF !(InternalSet Key child)
    | InternalStringF !(Const InternalString child)
    | StringLiteralF !(Const StringLiteral child)
    deriving stock (Eq, Ord, Show)
    deriving stock (Foldable, Functor, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (KeyF child) where
    unparse = unparseGeneric
    unparse2 = unparse2Generic

instance Synthetic (FreeVariables a) KeyF where
    synthetic = const emptyFreeVariables
    {-# INLINE synthetic #-}

instance Synthetic Sort KeyF where
    synthetic = \case
        ApplySymbolF application -> synthetic application
        InjF inj -> synthetic inj
        DomainValueF domainValue -> synthetic domainValue
        InternalBoolF internalBool -> synthetic internalBool
        InternalBytesF internalBytes -> synthetic internalBytes
        InternalIntF internalInt -> synthetic internalInt
        InternalListF internalList -> synthetic internalList
        InternalMapF internalMap -> synthetic internalMap
        InternalSetF internalSet -> synthetic internalSet
        InternalStringF internalString -> synthetic internalString
        StringLiteralF stringLiteral -> synthetic stringLiteral

instance From InternalBool (KeyF child) where
    from = InternalBoolF . Const
    {-# INLINE from #-}

instance From InternalBytes (KeyF child) where
    from = InternalBytesF . Const
    {-# INLINE from #-}

instance From InternalInt (KeyF child) where
    from = InternalIntF . Const
    {-# INLINE from #-}

instance From InternalString (KeyF child) where
    from = InternalStringF . Const
    {-# INLINE from #-}
