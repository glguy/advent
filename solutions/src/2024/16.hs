{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost, BangPatterns #-}
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
import Data.Array.Unboxed (UArray, assocs, (!), amap)
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
    let open = amap ('#' /=) input :: UArray Coord Bool
        start : _ = [p | (p,'S') <- assocs input]
        end   : _ = [p | (p,'E') <- assocs input]
        (p1, p2) = maze open start end
    --putStr (drawPicture (Map.fromSet (const '█') p2 <> Map.fromList (assocs input)))
    print p1
    print (length p2)

maze :: UArray Coord Bool -> Coord -> Coord -> (Int, Set Coord)
maze !open !start !end =
  case search step isDone (start, east) (Set.singleton start) of
    (cost, xs) -> (cost, Set.unions [path | (_, path) <- xs])
  where
    isDone (k, _) = k == end
    step (p, v) path =
      [(1001, (p', v'), Set.insert p' path) | let v' = turnRight v, let p' = p + v' , open ! p'] ++
      [(1001, (p', v'), Set.insert p' path) | let v' = turnLeft  v, let p' = p + v' , open ! p'] ++
      [(   1, (p', v ), Set.insert p' path) |                       let p' = p + v  , open ! p']

search ::
  (Ord k, Monoid v) =>
  (k -> v -> [(Int, k, v)]) ->
  (k -> Bool) ->
  k -> v ->
  (Int, [(k, v)])
search step isDone startK startV = go Set.empty (IntMap.singleton 0 (Map.singleton startK startV))
  where
    go seen q =
      case IntMap.minViewWithKey q of
        Nothing -> error "no solution"
        Just ((total, states), q1)
          | null dones -> go seen' q2
          | otherwise  -> (total, dones)
          where
            -- remove all the states at this cost that we've seen at a lower cost
            states' = Map.withoutKeys states seen

            -- look for states that have reached the target
            dones = [(k, v) | (k, v) <- Map.assocs states', isDone k]

            -- mark all the new states at this cost as seen so we don't revisit them again
            seen' = Set.union seen (Map.keysSet states')

            -- queue up all the successor states to be visited in the future
            q2 = IntMap.unionWith merge q1
               $ IntMap.fromListWith merge
                  [ (total + cost, Map.singleton k' v')
                    | (k, v) <- Map.assocs states'
                    , (cost, k', v') <- step k v
                  ]

            merge = Map.unionWith (<>)
