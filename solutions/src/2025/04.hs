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
import Advent.Coord ( neighbors, Coord )

-- | >>> :main
-- 1540
-- 8972
main :: IO ()
main =
 do input <- getInputMap 2025 4
    let rolls = Map.keysSet (Map.filter ('@' ==) input)
    let ns = removePaper rolls
    print (head ns)
    print (sum ns)

-- | Return the number of rolls removed each round of removals.
removePaper :: Set Coord -> [Int]
removePaper rolls
  | null elims = []
  | otherwise = length elims : removePaper (rolls Set.\\ elims)
  where elims = reachable rolls

-- | Find the subset of paper rolls that are reachable by a forklift.
reachable :: Set Coord -> Set Coord
reachable rolls = Set.filter (\x -> countBy (`Set.member` rolls) (neighbors x) < 4) rolls
