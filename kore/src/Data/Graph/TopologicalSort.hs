{- |
Module      : Data.Graph.TopologicalSort
Description : Topological sorting algorithm.
Copyright   : (c) Runtime Verification, 2018-2021
License     : BSD-3-Clause
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Data.Graph.TopologicalSort (
    topologicalSort,
    TopologicalSortCycles (..),
) where

import Data.Binary (Binary)
import Data.Graph (
    SCC (..),
    stronglyConnComp,
 )
import Data.Map.Strict qualified as Map
import GHC.Generics qualified as GHC
import Generics.SOP qualified as SOP
import Kore.Debug
import Prelude.Kore

newtype TopologicalSortCycles node = TopologicalSortCycles [node]
    deriving stock (Eq, GHC.Generic, Show)
    deriving anyclass (Binary)

instance SOP.Generic (TopologicalSortCycles node)

instance SOP.HasDatatypeInfo (TopologicalSortCycles node)

instance Debug node => Debug (TopologicalSortCycles node)

instance (Debug node, Diff node) => Diff (TopologicalSortCycles node)

{- | 'topologicalSort' sorts a graph topologically, starting with nodes which
have no 'next' nodes.
The graph is provided as a map form nodes to the list of their adjacent 'next'
nodes. All nodes must be present as keys in the map, even if they have no 'next'
nodes.
Returns an error for graphs that have cycles.
-}
topologicalSort ::
    Ord node =>
    Map.Map node [node] ->
    Either (TopologicalSortCycles node) [node]
topologicalSort edges =
    mapM
        extractSortedNode
        ( stronglyConnComp
            ( map
                (\(node, edges') -> (node, node, edges'))
                (Map.toList edges)
            )
        )
  where
    extractSortedNode :: SCC node -> Either (TopologicalSortCycles node) node
    extractSortedNode (AcyclicSCC n) = Right n
    extractSortedNode (CyclicSCC nodes) = Left (TopologicalSortCycles nodes)
