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
import Advent.Coord (Coord, east, turnRight, turnLeft)
import Data.Array.Unboxed (UArray, assocs, (!))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 88416
-- 442
main :: IO ()
main =
 do input <- getInputArray 2024 16
    let start:_ = [p | (p,'S') <- assocs input]
        q0 = IntMap.singleton 0 (Map.singleton (start, east) (Set.singleton start))
        (p1, p2) = search input Set.empty q0
    print p1
    print p2

search ::
  UArray Coord Char                       {- ^ input grid -} ->
  Set (Coord, Coord)                      {- ^ position/velocity pairs already finished -} ->
  IntMap (Map (Coord, Coord) (Set Coord)) {- ^ cost to (position/velocity to nodes-on-path -} ->
  (Int, Int)                              {- ^ cost of shortest path and nodes on shorts paths -}
search input seen q =
  case IntMap.minViewWithKey q of
    Nothing -> error "no solution"
    Just ((cost, states), q1)
      | not (null dones) -> (cost, Set.size (Set.unions dones))
      | otherwise        -> search input seen' q2
      where
        -- remove all the states at this cost that we've seen at a lower cost
        states' = Map.withoutKeys states seen

        -- look for states that have reached the target
        dones = [visited | ((p, _), visited) <- Map.assocs states', input ! p == 'E']

        -- mark all the new states at this cost as seen so we don't revisit them again
        seen' = Set.union seen (Map.keysSet states')

        -- queue up all the successor states to be visited in the future
        q2 = IntMap.unionWith merge q1
           $ IntMap.fromListWith merge
              [ next
                | ((p, v), path) <- Map.assocs states'
                , next <- [(cost + 1000, Map.singleton (p, turnRight v) path)]
                       ++ [(cost + 1000, Map.singleton (p, turnLeft  v) path)]
                       ++ [(cost +    1, Map.singleton (p', v) (Set.insert p' path))
                          | let p' = p + v, '#' /= input ! p'
                          ]
              ]

        merge = Map.unionWith Set.union
           
  