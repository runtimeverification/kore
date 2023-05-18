{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.Internal.Alias (
    Alias (..),
    toSymbolOrAlias,

    -- * Re-exports
    module Kore.Internal.ApplicationSorts,
) where

import Control.DeepSeq (
    deepseq,
 )
import Data.Binary (Binary)
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.AST.AstWithLocation
import Kore.Attribute.Pattern.FreeVariables (
    FreeVariables,
 )
import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Internal.ApplicationSorts
import Kore.Sort
import Kore.Syntax.Application
import Kore.Syntax.Variable
import Kore.Unparser
import Prelude.Kore

data Alias patternType = Alias
    { aliasConstructor :: !Id
    , aliasParams :: ![Sort]
    , aliasSorts :: !ApplicationSorts
    , aliasLeft :: [SomeVariable VariableName]
    , aliasRight :: patternType
    }
    deriving stock (Show)
    deriving stock (Functor, Foldable, Traversable)
    deriving stock (GHC.Generic)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Eq patternType => Eq (Alias patternType) where
    (==) a b =
        on (==) aliasConstructor a b
            && on (==) aliasParams a b
    {-# INLINE (==) #-}

instance Ord patternType => Ord (Alias patternType) where
    compare a b =
        on compare aliasConstructor a b
            <> on compare aliasParams a b

instance Hashable patternType => Hashable (Alias patternType) where
    hashWithSalt salt Alias{aliasConstructor, aliasParams} =
        salt `hashWithSalt` aliasConstructor `hashWithSalt` aliasParams

instance Unparse (Alias patternType) where
    unparse Alias{aliasConstructor, aliasParams} =
        unparse aliasConstructor
            <> parameters aliasParams

    unparse2 Alias{aliasConstructor} =
        unparse2 aliasConstructor

instance
    Ord variable =>
    Synthetic (FreeVariables variable) (Application (Alias patternType))
    where
    -- TODO (thomas.tuegel): Consider that there could be bound variables here.
    synthetic = fold
    {-# INLINE synthetic #-}

instance Synthetic Sort (Application (Alias patternType)) where
    synthetic application =
        resultSort & deepseq (matchSorts operandSorts children)
      where
        Application{applicationSymbolOrAlias = alias} = application
        Application{applicationChildren = children} = application
        Alias{aliasSorts} = alias
        resultSort = applicationSortsResult aliasSorts
        operandSorts = applicationSortsOperands aliasSorts

instance AstWithLocation (Alias patternType) where
    locationFromAst = locationFromAst . aliasConstructor

toSymbolOrAlias :: Alias patternType -> SymbolOrAlias
toSymbolOrAlias alias =
    SymbolOrAlias
        { symbolOrAliasConstructor = aliasConstructor alias
        , symbolOrAliasParams = aliasParams alias
        }
