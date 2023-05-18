{- |
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause

This module includes all the data structures necessary for representing
the syntactic categories of a Kore definition that do not need unified
constructs.

Unified constructs are those that represent both meta and object versions of
an AST term in a single data type (e.g. 'SymbolOrAlias' that can be either
'Symbol' or 'Alias')

Please refer to
<http://github.com/runtimeverification/haskell-backend/blob/master/docs/kore-syntax.md kore-syntax.md>.
-}
module Kore.AST.Common (
    MLPatternType (..),
    allPatternTypes,
    patternString,
) where

import Data.Binary (Binary)
import Data.String (
    fromString,
 )
import GHC.Generics (
    Generic,
 )
import Kore.Unparser
import Prelude.Kore

-- | Enumeration of patterns starting with @\\@
data MLPatternType
    = AndPatternType
    | BottomPatternType
    | CeilPatternType
    | DomainValuePatternType
    | EqualsPatternType
    | ExistsPatternType
    | FloorPatternType
    | ForallPatternType
    | IffPatternType
    | ImpliesPatternType
    | InPatternType
    | MuPatternType
    | NextPatternType
    | NotPatternType
    | NuPatternType
    | OrPatternType
    | RewritesPatternType
    | TopPatternType
    deriving stock (Show, Generic, Eq)
    deriving anyclass (Binary)

instance Hashable MLPatternType

instance Unparse MLPatternType where
    unparse = ("\\" <>) . fromString . patternString
    unparse2 = ("\\" <>) . fromString . patternString

allPatternTypes :: [MLPatternType]
allPatternTypes =
    [ AndPatternType
    , BottomPatternType
    , CeilPatternType
    , DomainValuePatternType
    , EqualsPatternType
    , ExistsPatternType
    , FloorPatternType
    , ForallPatternType
    , IffPatternType
    , ImpliesPatternType
    , InPatternType
    , MuPatternType
    , NextPatternType
    , NotPatternType
    , NuPatternType
    , OrPatternType
    , RewritesPatternType
    , TopPatternType
    ]

patternString :: MLPatternType -> String
patternString pt = case pt of
    AndPatternType -> "and"
    BottomPatternType -> "bottom"
    CeilPatternType -> "ceil"
    DomainValuePatternType -> "dv"
    EqualsPatternType -> "equals"
    ExistsPatternType -> "exists"
    FloorPatternType -> "floor"
    ForallPatternType -> "forall"
    IffPatternType -> "iff"
    ImpliesPatternType -> "implies"
    InPatternType -> "in"
    MuPatternType -> "mu"
    NextPatternType -> "next"
    NotPatternType -> "not"
    NuPatternType -> "nu"
    OrPatternType -> "or"
    RewritesPatternType -> "rewrites"
    TopPatternType -> "top"
