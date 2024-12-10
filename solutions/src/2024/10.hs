{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/10>

This solution solves the task by:
- recursively finding all valid hiking trails from each trailhead,
- filtering paths that meet the criteria for gradual uphill slopes,
- calculating the score of each trailhead (total unique endpoints),
- calculating the rating of each trailhead (distinct paths to any endpoint).

The solution uses a depth-first search approach for path exploration.

>>> :{
:main + "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
"
:}
36
81

-}
module Main (main) where

import Advent (arrIx, getInputArray, ordNub)
import Advent.Coord (Coord, cardinal)
import Data.Array.Unboxed (UArray, assocs)

-- | >>> :main
-- 778
-- 1925
main :: IO ()
main =
 do input <- getInputArray 2024 10
    let paths = [pathsFrom input start '0' | (start, '0') <- assocs input]
    print (length (concatMap ordNub paths))
    print (length (concat           paths))

-- | Find the list of coordinates of peaks reachable from each trail
-- starting from the given coordinate and height at that coordinate.
pathsFrom ::
    UArray Coord Char {- ^ topographic map                  -} ->
    Coord             {- ^ current location                 -} ->
    Char              {- ^ current height                   -} ->
    [Coord]           {- ^ list of reachable peak locations -}
pathsFrom _ i '9' = [i]
pathsFrom a i ai  = [k | j <- cardinal i, aj <- arrIx a j, succ ai == aj, k <- pathsFrom a j aj]
