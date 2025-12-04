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

import Advent ( getInputMap, countBy )
import Advent.Queue (Queue)
import Advent.Queue qualified as Queue
import Advent.Coord ( neighbors, Coord )

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

part1 :: Set Coord -> Int
part1 rolls = countBy (\x -> countBy (`Set.member` rolls) (neighbors x) < magic) rolls

part2 :: Set Coord -> Int
part2 rolls = length rolls - part2' (Queue.fromList (Set.toList rolls)) rolls

part2' :: Queue Coord -> Set Coord -> Int
part2' Queue.Empty rolls = length rolls
part2' (x Queue.:<| q) rolls
  | Set.notMember x rolls = part2' q rolls
  | length ns < magic     = part2' (Queue.appendList q ns) rolls'
  | otherwise             = part2' q rolls
  where
    ns     = filter (`Set.member` rolls) (neighbors x)
    rolls' = Set.delete x rolls
