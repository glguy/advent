{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/16>

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
    let start:_ = [p | (p,'S') <- assocs input]
        end  :_ = [p | (p,'E') <- assocs input]
        open    = amap ('#' /=) input

        -- start with all the possible initial facings so that the optimization later
        -- that assumes we only turn 90-degrees before moving will hold
        q0 = IntMap.fromList [(   0, Map.singleton (start, east) [])
                             ,(1000, Map.fromList [((start, north), []), ((start, south), [])])
                             ,(2000, Map.singleton (start, west) [])]

        step (p,v) = [(1001, (p', v')) | let v' = turnRight v, let p' = p + v', open ! p']
                  ++ [(1001, (p', v')) | let v' = turnLeft  v, let p' = p + v', open ! p']
                  ++ [(   1, (p', v )) |                       let p' = p + v , open ! p']
        isEnd (p, _) = p == end

        (cost, preds) = shortestPath step isEnd q0
        nodesOnShortestPaths
          = Set.map fst
          $ fillN (preds Map.!)
            [(end,v) | v <- [north, east, south, west], Map.member (end, v) preds]

    print cost
    print (length nodesOnShortestPaths)

-- | Mapping from a node to the predecessors of that node on the shortest path to that node
type Predecessors a = Map a [a]

-- | Main loop for a shortest-path implementation that computes the cost of the shortest path
shortestPath ::
  Ord a =>
  (a -> [(Int, a)])       {- ^ successors of a node -} ->
  (a -> Bool)             {- ^ predicate for the destination -} ->
  IntMap (Predecessors a) {- ^ cost to candidate predecessor additions -} ->
  (Int, Predecessors a)   {- ^ cost of shortest path to end and predecessors of all shortest paths -}
shortestPath = go Map.empty
  where
    go seen step isEnd q =
      case IntMap.minViewWithKey q of
        Nothing -> error "no solution"
        Just ((cost, states), q1)
          | done      -> (cost, seen')
          | otherwise -> go seen' step isEnd q2
          where
            -- remove all the states at this cost that we've seen at a lower cost
            states' = Map.difference states seen

            -- look for states that have reached the target
            done = any isEnd (Map.keys states')

            -- mark all the new states at this cost as seen so we don't revisit them again
            seen' = Map.union seen states'

            -- queue up all the successor states to be visited in the future
            q2 = foldl (\m (k,v) -> IntMap.insertWith (Map.unionWith (++)) k v m) q1
              [(cost + amt, Map.singleton next [cur]) | cur <- Map.keys states', (amt, next) <- step cur]
