{- |
Module      : Kore.Attribute.Sort
Description : Sort sentence attributes
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Attribute.Sort (
    Sort (..),
) where

import Control.Monad qualified as Monad
import Data.Binary (Binary)
import Data.Generics.Product
import Kore.Attribute.Hook
import Kore.Attribute.Parser hiding (
    Sort,
 )
import Kore.Attribute.Smtlib.Smtlib
import Kore.Attribute.Sort.Concat
import Kore.Attribute.Sort.Element
import Kore.Attribute.Sort.HasDomainValues (
    HasDomainValues,
 )
import Kore.Attribute.Sort.Unit
import Prelude.Kore hiding (
    concat,
 )

data Sort = Sort
    { hook :: !Hook
    -- ^ The builtin sort hooked to the sort.
    , smtlib :: !Smtlib
    -- ^ The user-defined translation of the sort for SMT.
    , unit :: !Unit
    -- ^ The unit symbol associated with the sort.
    , element :: !Element
    -- ^ The element symbol associated with the sort.
    , concat :: !Concat
    -- ^ The concat symbol associated with the sort.
    , hasDomainValues :: !HasDomainValues
    -- ^ whether the sort has domain values
    }
    deriving stock (Eq, Generic, Ord, Show)
    deriving anyclass (Binary)

instance NFData Sort

defaultSortAttributes :: Sort
defaultSortAttributes =
    Sort
        { hook = def
        , smtlib = def
        , unit = def
        , element = def
        , concat = def
        , hasDomainValues = def
        }

-- | See also: 'defaultSortAttributes'
instance Default Sort where
    def = defaultSortAttributes

instance ParseAttributes Sort where
    parseAttribute attr =
        typed @Hook (parseAttribute attr)
            Monad.>=> typed @Smtlib (parseAttribute attr)
            Monad.>=> typed @Unit (parseAttribute attr)
            Monad.>=> typed @Element (parseAttribute attr)
            Monad.>=> typed @Concat (parseAttribute attr)
            Monad.>=> typed @HasDomainValues (parseAttribute attr)

instance From Sort Attributes where
    from =
        mconcat
            . sequence
                [ toAttributes . hook
                , toAttributes . smtlib
                , toAttributes . unit
                , toAttributes . element
                , toAttributes . concat
                , toAttributes . hasDomainValues
                ]
