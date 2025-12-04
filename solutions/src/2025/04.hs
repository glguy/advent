{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/4>

>>> :{
:main +
"..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
"
:}
13
43

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Advent (getInputMap, countBy)
import Advent.Coord (neighbors, Coord)
import Advent.Queue (Queue)
import Advent.Queue qualified as Queue

-- | >>> :main
-- 1540
-- 8972
main :: IO ()
main =
 do input <- getInputMap 2025 4
    let rolls = Map.keysSet (Map.filter ('@' ==) input)
    print (part1 rolls)
    print (part2 rolls)

-- | The magic number of neighbors that are needed to obstruct a location.
magic :: Int
magic = 4

-- | Finds the number of rolls that start in a removable state.
part1 :: Set Coord -> Int
part1 rolls = countBy (\x -> countBy (`Set.member` rolls) (neighbors x) < magic) rolls

-- | Finds the number of rolls that can be removed allowing for
-- repeated iteration of removals.
part2 :: Set Coord -> Int
part2 rolls = length rolls - length rolls'
  where
    rolls' = part2' (Queue.fromList (Set.toList rolls)) rolls

-- | Worker for finding and removing accessible rolls. The queue
-- tracks all the locations that still need to be considered.
-- When a roll is removed its neighbors are added to the queue
-- to be considered.
part2' :: Queue Coord -> Set Coord -> Set Coord
part2' Queue.Empty     rolls = rolls
part2' (x Queue.:<| q) rolls
  | Set.notMember x rolls = part2' q rolls
  | length ns < magic     = part2' (Queue.appendList q ns) rolls'
  | otherwise             = part2' q rolls
  where
    ns     = filter (`Set.member` rolls) (neighbors x)
    rolls' = Set.delete x rolls
