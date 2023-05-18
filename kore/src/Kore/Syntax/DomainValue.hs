{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Syntax.DomainValue (
    DomainValue (..),
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Pattern.FreeVariables
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Sort
import Kore.TopBottom
import Kore.Unparser
import Prelude.Kore
import Pretty qualified

{- | 'DomainValue' corresponds to the @\\dv@ branch of the @matching-logic-pattern@ syntactic category from <https://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md#patterns kore-syntax.md#patterns>.

'domainValueSort' is the sort of the result.

This represents the encoding of an object constant, e.g. we may use
@\\dv{Int{}}{"123"}@ instead of a representation based on constructors,
e.g. @succesor(succesor(...succesor(0)...))@
-}
data DomainValue sort child = DomainValue
    { domainValueSort :: !sort
    , domainValueChild :: !child
    }
    deriving stock (Eq, Ord, Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (DomainValue Sort child) where
    unparse DomainValue{domainValueSort, domainValueChild} =
        "\\dv"
            <> parameters [domainValueSort]
            <> Pretty.parens (unparse domainValueChild)

    unparse2 DomainValue{domainValueSort, domainValueChild} =
        "\\dv"
            <> parameters2 [domainValueSort]
            Pretty.<+> unparse domainValueChild

instance Synthetic (FreeVariables variable) (DomainValue sort) where
    synthetic = domainValueChild
    {-# INLINE synthetic #-}

instance Synthetic Sort (DomainValue Sort) where
    synthetic DomainValue{domainValueSort, domainValueChild} =
        domainValueSort
            & seq (sameSort stringMetaSort domainValueChild)
    {-# INLINE synthetic #-}

instance TopBottom a => TopBottom (DomainValue Sort a) where
    isTop _ = False
    isBottom DomainValue{domainValueChild} = isBottom domainValueChild
