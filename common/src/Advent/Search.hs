{-# Language LambdaCase, ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Advent.Search
Description : Generalized graph search
Copyright   : (c) Eric Mertens, 2019-2021
License     : ISC
Maintainer  : emertens@gmail.com

These implementations provide a lazily-generated list of visited
states with the order defined by the search strategy.

-}
module Advent.Search (
  -- * Depth-first search
  dfs, dfsN, dfsOn, dfsOnN,

  -- * Breadth-first search
  bfs, bfsN, bfsOn, bfsOnN,

  -- * A* search
  AStep(..),
  astar, astarN, astarOn, astarOnN,

  -- * Reachable exploration
  fill, fillN, fillInt, fillNInt,

  ) where

import Advent.PQueue qualified as PQueue
import Advent.Queue qualified as Queue
import Data.Foldable (foldl')
import Data.Set qualified as Set
import Data.IntSet qualified as IntSet

-- | Shortcut for @'dfsOn' 'id'@
dfs :: Ord a => (a -> [a]) -> a -> [a]
dfs = dfsOn id
{-# INLINE dfs #-}

-- | Shortcut for @'dfsOnN' 'id'@
dfsN :: Ord a => (a -> [a]) -> [a] -> [a]
dfsN = dfsOnN id
{-# INLINE dfsN #-}

-- | Depth-first search.
--
-- Generates the list of unique visited states from a
-- given starting state. States are unique up to the
-- characterizing function.
dfsOn ::
  Ord r =>
  (a -> r)   {- ^ state characterization              -} ->
  (a -> [a]) {- ^ successors function                 -} ->
  a          {- ^ initial state                       -} ->
  [a]        {- ^ visited states in depth-first order -}
dfsOn rep next start = dfsOnN rep next [start]
{-# INLINE dfsOn #-}

-- | Depth-first search.
--
-- Generates the list of unique visited states from a
-- given starting state. States are unique up to the
-- characterizing function.
dfsOnN ::
  Ord r =>
  (a -> r)   {- ^ state characterization              -} ->
  (a -> [a]) {- ^ successors function                 -} ->
  [a]        {- ^ initial states                      -} ->
  [a]        {- ^ visited states in depth-first order -}
dfsOnN rep next = loop Set.empty
  where
    loop !seen = \case
      [] -> []
      x:xs
        | Set.member r seen ->     loop seen xs
        | otherwise         -> x : loop seen1 (next x ++ xs)
        where
          r     = rep x
          seen1 = Set.insert r seen

-- | Shortcut for @'bfsOn' 'id'@
bfs :: Ord a => (a -> [a]) -> a -> [a]
bfs = bfsOn id
{-# INLINE bfs #-}

-- | Shortcut for @'bfsOnN' 'id'@
bfsN :: Ord a => (a -> [a]) -> [a] -> [a]
bfsN = bfsOnN id
{-# INLINE bfsN #-}

-- | Enumerate the reachable states in breadth-first order
-- given a successor state function and initial state.
--
-- States are compared for equality using the representative
-- function. If the representatives are equal the state is
-- considered already visited.
{-# INLINE [0] bfsOn #-}
bfsOn ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  a          {- ^ initial state             -} ->
  [a]        {- ^ reachable states          -}
bfsOn rep next start = bfsOnN rep next [start]

-- | Generalization of 'bfsOn' allowing multiple
-- initial states to be considered.
bfsOnN ::
  Ord r =>
  (a -> r)   {- ^ representative function   -} ->
  (a -> [a]) {- ^ successor state generator -} ->
  [a]        {- ^ initial states            -} ->
  [a]        {- ^ reachable states          -}
bfsOnN rep next start = loop Set.empty (Queue.fromList start)
  where
    loop !seen = \case
      Queue.Empty -> []
      x Queue.:<| q
        | Set.member r seen ->     loop seen  q
        | otherwise         -> x : loop seen' q'
        where
          r     = rep x
          seen' = Set.insert r seen
          q'    = Queue.appendList q (next x)
{-# INLINE [0] bfsOnN #-}

{-# RULES "bfsOn/Int" bfsOn = bfsOnInt #-}
{-# INLINE bfsOnInt #-}
bfsOnInt :: (a -> Int) -> (a -> [a]) -> a -> [a]
bfsOnInt rep next start = loop IntSet.empty (Queue.singleton start)
  where
    loop !seen = \case
      Queue.Empty -> []
      x Queue.:<| q
        | IntSet.member r seen ->     loop seen  q
        | otherwise            -> x : loop seen' q'
        where
          r     = rep x
          seen' = IntSet.insert r seen
          q'    = Queue.appendList q (next x)

-- | Shortcut for @'astarOn' 'id'@
astar :: Ord a => (a -> [AStep a]) -> a -> [(a,Int)]
astar = astarOn id
{-# INLINE astar #-}

-- | Shortcut for @'astarOnN' 'id'@
astarN :: Ord a => (a -> [AStep a]) -> [a] -> [(a,Int)]
astarN = astarOnN id
{-# INLINE astarN #-}

-- | A* graph search producing a list of reached states and the
-- minimum cost of reaching that state.
--
-- Returned states will be unique up to the characterization function.
-- This allows extra information of a node to be ignored for the
-- purposes of the search. For example, a node might remember the
-- path used to reach it while for the search the particular path
-- taken might not matter.
astarOn ::
  Ord b =>
  (a -> b)         {- ^ state characterization                                   -} ->
  (a -> [AStep a]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  a                {- ^ starting state                                           -} ->
  [(a,Int)]        {- ^ list of states visited                                   -}
astarOn rep nexts start = astarOnN rep nexts [start]

-- | Generalization of 'astarOn' that accepts multiple starting states.
astarOnN ::
  Ord b =>
  (a -> b)         {- ^ state characterization                                   -} ->
  (a -> [AStep a]) {- ^ step function (new state, step cost, distance heuristic) -} ->
  [a]              {- ^ starting states                                          -} ->
  [(a,Int)]        {- ^ list of states visited                                   -}
astarOnN rep nexts starts = go Set.empty (PQueue.fromList [(0, WC 0 s) | s <- starts])
  where
    go !seen = \case
      PQueue.Empty -> []
      WC cost x PQueue.:<| work
        | Set.member r seen -> go seen work
        | otherwise         -> (x,cost) : go seen' work'
        where
          r     = rep x
          seen' = Set.insert r seen
          work' = foldl' addWork work (nexts x)
          addWork w (AStep x' stepcost heuristic) =
            PQueue.insert (cost' + heuristic) (WC cost' x') w
            where
              cost' = cost + stepcost
{-# INLINE astarOn #-}

-- Helper type to unpack the cost value in the A* priority queue
data WithCost a = WC !Int a

-- | A step in the A* graph search annotated with its cost and an
-- estimate of the distance remaining to the goal. The estimate
-- must be an underapproximation to ensure the search finds the
-- optimal solution
data AStep a = AStep {
  astepNext      :: a,    -- ^ successor node
  astepCost      :: !Int, -- ^ cost of edge
  astepHeuristic :: !Int  -- ^ heuristic cost to goal from this new node
  } deriving Show

-- | Generate a set of all the values reachable from a starting
-- state and a step function.
fill :: Ord a => (a -> [a]) -> a -> Set.Set a
fill step x = fillN step [x]
{-# INLINE fill #-}

-- | Generate a set of all the values reachable from a list
-- of starting states and a step function.
fillN :: Ord a => (a -> [a]) -> [a] -> Set.Set a
fillN step = foldl' go Set.empty
  where
    go seen x
      | x `Set.member` seen = seen
      | otherwise = foldl' go (Set.insert x seen) (step x)
{-# INLINE fillN #-}

-- | Generate a set of all the values reachable from a starting
-- state and a step function. Specialized version of 'fill' when
-- working with 'Int'.
fillInt :: (Int -> [Int]) -> Int -> IntSet.IntSet
fillInt step x = fillNInt step [x]
{-# INLINE fillInt #-}

-- | Generate a set of all the values reachable from a list
-- of starting states and a step function. Specialized version
-- of 'fillN' when working with 'Int'.
fillNInt :: (Int -> [Int]) -> [Int] -> IntSet.IntSet
fillNInt step = foldl' go IntSet.empty
  where
    go seen x
      | x `IntSet.member` seen = seen
      | otherwise = foldl' go (IntSet.insert x seen) (step x)
{-# INLINE fillNInt #-}
