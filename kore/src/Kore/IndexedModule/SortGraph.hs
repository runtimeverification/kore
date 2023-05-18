{- |
Copyright   : (c) Runtime Verification, 2019-2021
License     : BSD-3-Clause
-}
module Kore.IndexedModule.SortGraph (
    SortGraph,
    fromSubsorts,
    fromIndexedModule,
    subsortsOf,
    isSubsortOf,
    --
    Subsort (..),
) where

import Data.Binary (Binary)
import Data.Graph.Inductive.Graph qualified as Graph
import Data.Graph.Inductive.PatriciaTree (
    Gr,
 )
import Data.Graph.Inductive.Query.DFS qualified as Graph.Query.DFS
import Data.Map.Strict (
    Map,
 )
import Data.Map.Strict qualified as Map
import Data.Set (
    Set,
 )
import Data.Set qualified as Set
import Data.Vector qualified as Vector
import Debug
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Attribute.Subsort (
    Subsort (..),
 )
import Kore.IndexedModule.IndexedModule (
    IndexedModule,
    indexedModuleSubsorts,
 )
import Kore.Sort
import Prelude.Kore

-- | 'SortGraph' represents the partial order on sorts.
newtype SortGraph = SortGraph {unSortGraph :: Map Sort (Set Sort)}
    deriving stock (GHC.Generic, Typeable)
    deriving anyclass (Binary)
    deriving anyclass (NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

-- | Build a 'SortGraph' from a list of 'Subsort' relations.
fromSubsorts ::
    -- | direct subsort relations
    [Subsort] ->
    SortGraph
fromSubsorts relations =
    let supersorts = supersort <$> relations
        subsorts = subsort <$> relations
        sorts = supersorts <> subsorts
        nodes = Vector.fromList sorts

        lnodes :: [Graph.LNode Sort]
        lnodes = Vector.toList $ Vector.indexed nodes

        nodeMap :: Map Sort Int
        nodeMap = Map.fromList (map swap lnodes)

        edges :: [Graph.LEdge ()]
        edges =
            [ (nodeMap Map.! supersort, nodeMap Map.! subsort, ())
            | Subsort{supersort, subsort} <- relations
            ]

        sortGraph :: Gr Sort ()
        sortGraph = Graph.mkGraph lnodes edges

        reachable :: Sort -> Set Sort
        reachable sort =
            let node = nodeMap Map.! sort
                children = Graph.Query.DFS.reachable node sortGraph
                childrenSorts = map (nodes Vector.!) children
             in Set.fromList childrenSorts
     in SortGraph $ Map.fromSet reachable (Set.fromList supersorts)

fromIndexedModule ::
    IndexedModule patternType symbolAttrs axiomAttrs ->
    SortGraph
fromIndexedModule = fromSubsorts . indexedModuleSubsorts
{-# INLINE fromIndexedModule #-}

-- | Find the transitive subsorts of the given 'Sort'.
subsortsOf :: SortGraph -> Sort -> Set Sort
subsortsOf (SortGraph subsortMap) sort =
    Map.lookup sort subsortMap
        -- If the Sort is not present in the map, it was not the supersort in any of
        -- the relations used to construct the SortGraph.
        & fromMaybe Set.empty

-- | Is the first 'Sort' a subsort (transitively) of the second?
isSubsortOf ::
    SortGraph ->
    -- | (conjectured) subsort
    Sort ->
    -- | (conjectured) supersort
    Sort ->
    Bool
isSubsortOf sortGraph psub psuper =
    Set.member psub (subsortsOf sortGraph psuper)
