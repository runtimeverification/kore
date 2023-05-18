{- |
Copyright   : (c) Runtime Verification, 2020-2021
License     : BSD-3-Clause
-}
module Kore.IndexedModule.OverloadGraph (
    OverloadGraph,
    getOverloaded,
    getOverloading,
    isOverloaded,
    isOverloading,
    commonOverloads,
    fromIndexedModule,
    --
    fromOverloads,
) where

import Data.Binary (Binary)
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Axiom qualified as Attribute
import Kore.Attribute.Symbol qualified as Attribute
import Kore.IndexedModule.IndexedModule (
    VerifiedModule,
    recursiveIndexedModuleAxioms,
 )
import Kore.Internal.Symbol
import Prelude.Kore

-- | 'OverloadGraph' maps symbols to symbols overloading them and the reverse
data OverloadGraph = OverloadGraph
    { overloadingSymbols :: !(Map Symbol (Set Symbol))
    -- ^ maps a symbol to the symbols overloading it
    , overloadedSymbols :: !(Map Symbol (Set Symbol))
    -- ^ maps a symbol to the symbols overloaded by it
    }
    deriving stock (GHC.Generic, Typeable)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | Whether the symbol is an overloading symbol
isOverloaded :: OverloadGraph -> Symbol -> Bool
isOverloaded graph s = s `Map.member` overloadingSymbols graph

-- | Whether the first symbol overloads the second
isOverloading :: OverloadGraph -> Symbol -> Symbol -> Bool
isOverloading graph s1 s2
    | Just ss <- overloadingSymbols graph Map.!? s2 =
        s1 `Set.member` ss
    | otherwise = False

-- | List of symbols overloading both symbol arguments
commonOverloads :: OverloadGraph -> Symbol -> Symbol -> [Symbol]
commonOverloads graph sym1 sym2 =
    maybe
        []
        Set.toList
        ( Set.intersection
            <$> (overloadingSymbols graph Map.!? sym1)
            <*> (overloadingSymbols graph Map.!? sym2)
        )

getOverloaded :: OverloadGraph -> Symbol -> Set Symbol
getOverloaded graph sym1 =
    fromMaybe Set.empty (overloadedSymbols graph Map.!? sym1)

getOverloading :: OverloadGraph -> Symbol -> Set Symbol
getOverloading graph sym1 =
    fromMaybe Set.empty (overloadingSymbols graph Map.!? sym1)

{- | Build a 'OverloadGraph' from a list of overloaded symbol pairs.

  The list of overloaded symbol pairs given as an argument is assumed
  to represent the _transitive closure_ of the overloading relation.
-}
fromOverloads ::
    -- | first symbol overloads the second
    [(Symbol, Symbol)] ->
    OverloadGraph
fromOverloads overloadPairsList =
    let allOverloadsList = concatMap (\(o1, o2) -> [o1, o2]) overloadPairsList
        allOverloadsSet = Set.fromList allOverloadsList
     in OverloadGraph
            { overloadingSymbols = Map.fromSet superOverloading allOverloadsSet
            , overloadedSymbols = Map.fromSet subOverloading allOverloadsSet
            }
  where
    superOverloading subOverload =
        Set.fromList [x | (x, y) <- overloadPairsList, y == subOverload]
    subOverloading superOverload =
        Set.fromList [y | (x, y) <- overloadPairsList, x == superOverload]

{- | Builds an overload graph from the @overload@ attribute annotations
associated to overloading equations in a verified module.

Assumes the overloading relation is already transitive.
-}
fromIndexedModule ::
    VerifiedModule Attribute.Symbol ->
    OverloadGraph
fromIndexedModule verifiedModule = fromOverloads overloadPairList
  where
    overloadPairList = preOverloadPairsList
    preOverloadPairsList =
        mapMaybe
            (Attribute.getOverload . Attribute.overload . fst)
            (recursiveIndexedModuleAxioms verifiedModule)
