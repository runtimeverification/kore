{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.Internal.SideCondition.SideCondition (
    Representation,
    mkRepresentation,
) where

import Data.Binary (Binary (..))
import Data.Hashable (
    Hashed,
    hashed,
    unhashed,
 )
import Data.Type.Equality (
    testEquality,
    (:~:) (..),
 )
import Kore.Debug (
    Debug (..),
    Diff (..),
 )
import Prelude.Kore
import Pretty (
    Pretty (..),
 )
import Type.Reflection (
    SomeTypeRep (..),
    TypeRep,
    typeRep,
    withTypeable,
 )

-- import Data.Constraint -- constraints package

data Representation where
    Representation ::
        (Ord a, Pretty a, Binary a, Typeable a) =>
        !(TypeRep a) ->
        -- FIXME should store a Dict ((Ord a, Pretty a, Binary a, Typeable a)) instead.
        -- This can be Shown and Read for the Binary instance, and then brought
        -- into scope with withDict to get the Hashed a and construct the result.
        Hashed a ->
        Representation

instance Binary Representation where
    put (Representation typeRep1 x) = do
        put $ SomeTypeRep typeRep1 -- FIXME write Dict here
        put $ unhashed x
    get = do
        SomeTypeRep rep <- get @SomeTypeRep -- FIXME read Dict here
        withTypeable rep $ do -- FIXME use withDict here
            h <- get @() -- wrong type
            -- type check fails below
            pure $ Representation rep (hashed h)

instance Eq Representation where
    (==) (Representation typeRep1 hashed1) (Representation typeRep2 hashed2) =
        case testEquality typeRep1 typeRep2 of
            Nothing -> False
            Just Refl -> hashed1 == hashed2
    {-# INLINE (==) #-}

instance Ord Representation where
    compare
        (Representation typeRep1 hashed1)
        (Representation typeRep2 hashed2) =
            case testEquality typeRep1 typeRep2 of
                Nothing -> compare (SomeTypeRep typeRep1) (SomeTypeRep typeRep2)
                Just Refl -> compare hashed1 hashed2
    {-# INLINE compare #-}

instance Show Representation where
    showsPrec prec (Representation typeRep1 _) =
        showParen (prec >= 10) $
            showString "Representation " . shows typeRep1 . showString " _"
    {-# INLINE showsPrec #-}

instance Hashable Representation where
    hashWithSalt salt (Representation typeRep1 hashed1) =
        salt `hashWithSalt` typeRep1 `hashWithSalt` hashed1
    {-# INLINE hashWithSalt #-}

instance NFData Representation where
    rnf (Representation typeRep1 hashed1) = typeRep1 `seq` hashed1 `seq` ()
    {-# INLINE rnf #-}

instance Pretty Representation where
    pretty (Representation _ h) = pretty (unhashed h)

{- | Creates a 'Representation'. Should not be used directly.
 See 'Kore.Internal.SideCondition.toRepresentation'.
-}
mkRepresentation :: (Ord a, Hashable a, Typeable a, Pretty a, Binary a) => a -> Representation
mkRepresentation = Representation typeRep . hashed

instance Debug Representation where
    debugPrec _ _ = "_"
    {-# INLINE debugPrec #-}

instance Diff Representation where
    diffPrec _ _ = Nothing
    {-# INLINE diffPrec #-}
