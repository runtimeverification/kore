{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Internal.Inj (
    Inj (..),
    toSymbol,
    toApplication,

    -- * Exceptions
    UnorderedInj (..),
    unorderedInj,
) where

import Control.Exception (
    Exception (..),
    throw,
 )
import Data.Binary (Binary)
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
 )
import Kore.Attribute.Symbol qualified as Attribute
import Kore.Attribute.Synthetic (
    Synthetic (..),
 )
import Kore.Internal.ApplicationSorts
import Kore.Internal.Symbol
import Kore.Sort
import Kore.Syntax.Application (
    Application (..),
 )
import Kore.TopBottom (
    TopBottom (..),
 )
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

data Inj a = Inj
    { injConstructor :: !Id
    , injFrom :: !Sort
    , injTo :: !Sort
    , injAttributes :: !Attribute.Symbol
    , injChild :: !a
    }
    deriving stock (Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Eq a => Eq (Inj a) where
    (==) a@(Inj _ _ _ _ _) b =
        on (==) injConstructor a b
            && on (==) injFrom a b
            && on (==) injTo a b
            && on (==) injChild a b
    {-# INLINE (==) #-}

instance Ord a => Ord (Inj a) where
    compare a@(Inj _ _ _ _ _) b =
        on compare injConstructor a b
            <> on compare injFrom a b
            <> on compare injTo a b
            <> on compare injChild a b
    {-# INLINE compare #-}

instance Hashable a => Hashable (Inj a) where
    hashWithSalt salt inj@(Inj _ _ _ _ _) =
        salt
            `hashWithSalt` injConstructor
            `hashWithSalt` injFrom
            `hashWithSalt` injTo
            `hashWithSalt` injChild
      where
        Inj{injConstructor, injFrom, injTo, injChild} = inj

instance Unparse a => Unparse (Inj a) where
    unparse inj = Pretty.hsep ["/* Inj: */", unparse (toApplication inj)]
    unparse2 inj = Pretty.hsep ["/* Inj: */", unparse2 (toApplication inj)]

instance Synthetic Sort Inj where
    synthetic Inj{injFrom, injTo, injChild} =
        injTo & seq (sameSort injFrom injChild)
    {-# INLINE synthetic #-}

instance Synthetic (FreeVariables variable) Inj where
    synthetic = injChild
    {-# INLINE synthetic #-}

instance TopBottom a => TopBottom (Inj a) where
    isTop _ = False
    isBottom Inj{injChild} = isBottom injChild

toSymbol :: Inj a -> Symbol
toSymbol inj@(Inj _ _ _ _ _) =
    Symbol
        { symbolConstructor = injConstructor
        , symbolParams = [injFrom, injTo]
        , symbolSorts = ApplicationSorts [injFrom] injTo
        , symbolAttributes = injAttributes
        }
  where
    Inj{injConstructor, injFrom, injTo, injAttributes} = inj

toApplication :: Inj a -> Application Symbol a
toApplication inj =
    Application
        { applicationSymbolOrAlias = toSymbol inj
        , applicationChildren = [injChild inj]
        }

{- | 'UnorderedInj' is thrown when the inner and outer sort are not ordered

The inner sort must be a subsort of the outer sort.
-}
newtype UnorderedInj = UnorderedInj (Inj ())
    deriving stock (Show, Typeable)

instance Exception UnorderedInj where
    displayException (UnorderedInj inj) =
        show $ "Unordered sort injection:" Pretty.<+> unparse (toSymbol inj)

-- | Throw an 'UnorderedInj' exception.
unorderedInj :: Inj a -> error
unorderedInj inj = throw (UnorderedInj inj{injChild = ()})
