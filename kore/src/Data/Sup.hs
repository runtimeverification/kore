{-|
Module      : Data.Sup
Description : Extend ordered types with a least upper bound
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

-}

module Data.Sup
    ( Sup (..)
    ) where

import Prelude.Kore

import Data.Data
    ( Data
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Pretty
    ( Pretty (..)
    )

{- | @Sup a@ is an extension of @a@ with a least upper bound.

If @a@ already has a least upper bound, 'Sup' is greater than that bound.

 -}
data Sup a
    = Element !a
    | Sup  -- ^ least upper bound (supremum)
    deriving (Read, Show)
    deriving (Data, Typeable)
    deriving (Functor)
    deriving (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)

instance Eq a => Eq (Sup a) where
    (==) Sup         = \case { Sup       -> True  ; _ -> False }
    (==) (Element a) = \case { Element b -> a == b; _ -> False }

instance Ord a => Ord (Sup a) where
    compare Sup         = \case { Sup       -> EQ         ; _   -> GT }
    compare (Element a) = \case { Element b -> compare a b; Sup -> LT }

-- | 'Sup' is the annihilator of 'Element'.
instance Ord a => Semigroup (Sup a) where
    (<>) a b = max <$> a <*> b

-- | 'Sup' is the annihilator of 'Element'.
instance Applicative Sup where
    pure = Element
    (<*>) Sup         = const Sup
    (<*>) (Element f) = \case { Sup -> Sup; Element a -> Element (f a) }

instance Pretty a => Pretty (Sup a) where
    pretty (Element a) = pretty a
    pretty Sup = mempty
