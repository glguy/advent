{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/16>

>>> :{
:main + "#######
#..S..#
#.###.#
#.##..#
#..E.##
#######
"
:}
4007
14

>>> :{
:main + "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
"
:}
7036
45

>>> :{
:main + "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
"
:}
11048
64

-}
module Main (main) where

import Advent (getInputArray)
import Advent.Coord (north, east, south, west, turnRight, turnLeft)
import Data.Array.Unboxed (assocs, amap, (!))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Advent.Search (fillN)

-- | >>> :main
-- 88416
-- 442
main :: IO ()
main =
 do input <- getInputArray 2024 16
    let start = head [p | (p,'S') <- assocs input]
        end   = head [p | (p,'E') <- assocs input]
        open  = amap ('#' /=) input

        step (p,v) = [(1000, (p, turnRight v))]
                  ++ [(1000, (p, turnLeft  v))]
                  ++ [(   1, (p', v)) | let p' = p + v, open ! p']
        isEnd (p, _) = p == end

        Just (cost, ends, preds) = shortestPath (start, east) step isEnd
        nodesOnShortestPaths = Set.map fst (fillN (preds Map.!) ends)

    print cost
    print (length nodesOnShortestPaths)

-- | Main loop for a shortest-path implementation that computes the cost of the shortest path.
shortestPath ::
  Ord a =>
  a                           {- ^ initial node -} ->
  (a -> [(Int, a)])           {- ^ successors of a node -} ->
  (a -> Bool)                 {- ^ predicate for the destination -} ->
  Maybe (Int, [a], Map a [a]) {- ^ cost, reached end states, predecessors -}
shortestPath start step isEnd = go Map.empty (IntMap.singleton 0 (Map.singleton start []))
  where
    addWork q (k, v) = IntMap.insertWith (Map.unionWith (++)) k v q
    go seen q =
      case IntMap.minViewWithKey q of
        Nothing -> Nothing
        Just ((cost, states), q1)
          | null done -> go seen' q2
          | otherwise -> Just (cost, done, seen')
          where
            -- remove all the states at this cost that we've seen at a lower cost
            states' = Map.difference states seen

            -- look for states that have reached the target
            done = filter isEnd (Map.keys states')

            -- mark all the new states at this cost as seen so we don't revisit them again
            seen' = Map.union seen states'

            -- queue up all the successor states to be visited in the future
            q2 = foldl addWork q1
              [(cost + amt, Map.singleton next [cur]) | cur <- Map.keys states', (amt, next) <- step cur]
