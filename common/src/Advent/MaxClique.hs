{-# Language ImportQualifiedPost #-}
{-|
Module      : Advent.MaxClique
Description : Maximal clique enumerator
Copyright   : (c) Eric Mertens, 2018-2021
License     : ISC
Maintainer  : emertens@gmail.com

Implementation of <https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm>

-}
module Advent.MaxClique where

import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet

-- | Find the maximal cliques of a graph.
maxCliques ::
  (a -> a -> Bool) {- ^ test for edge between nodes -} ->
  [a]              {- ^ all nodes -} ->
  [[a]]            {- ^ maximal cliques -}
maxCliques adj xs = map recover (maxCliquesInt (edges IntMap.!) (IntMap.keysSet nodes))
  where
    recover = IntMap.elems . IntMap.restrictKeys nodes
    nodes = IntMap.fromList (zip [0..] xs)
    edges =
      IntMap.mapWithKey
        (\i v ->
          IntMap.keysSet
            (IntMap.filterWithKey (\j u -> i /= j && adj v u) nodes))
        nodes

-- | Bron-Kerbosh algorithm on graphs labeled with integers. The graph should have no self-edges.
maxCliquesInt ::
  (Int -> IntSet) {- ^ node to adjacent nodes -} ->
  IntSet          {- ^ all nodes -} ->
  [IntSet]        {- ^ maximal cliques -}
maxCliquesInt neighbors nodes = top IntSet.empty IntSet.empty nodes []
  where
    top r x p
      | IntSet.null p, IntSet.null x = (r:)
      | otherwise = loop (IntSet.elems p') r x p 
      where
        pivot:_ = IntSet.elems p
        p' = p IntSet.\\ neighbors pivot

    loop []     _ _ _ = id
    loop (v:vs) r x p 
      = top (IntSet.insert v r) (IntSet.intersection ns x) (IntSet.intersection ns p)
      . loop vs r (IntSet.insert v x) (IntSet.delete v p)
      where ns = neighbors v
