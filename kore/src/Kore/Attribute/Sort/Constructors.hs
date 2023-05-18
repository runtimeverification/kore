{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Attribute.Sort.Constructors (
    Constructors (..),
    ConstructorLike (..),
    Constructor (..),
) where

import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Kore.Internal.Symbol (
    Symbol,
 )
import Kore.Sort (
    Sort,
 )
import Prelude.Kore

data Constructor = Constructor
    { name :: !Symbol
    , sorts :: ![Sort]
    }
    deriving stock (Show, Eq)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)

data ConstructorLike
    = ConstructorLikeConstructor !Constructor
    | ConstructorLikeInjection
    deriving stock (Show, Eq)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)

{- | @Nothing@ means that the sort has no constructors that we can recognize.
@Just value@ means that we recognized the sort's constructors, and @value@
contains the list of these constructors.
-}
newtype Constructors = Constructors {getConstructors :: Maybe (NonEmpty ConstructorLike)}
    deriving stock (Show, Eq)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
