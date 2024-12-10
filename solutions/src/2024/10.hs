{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/10>

The solution uses caching via `Map` to aggregate trail multiplicities
efficiently.

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

import Advent (arrIx, getInputArray)
import Advent.Coord (Coord, cardinal)
import Data.Array.Unboxed (UArray, assocs)
import Data.Map qualified as Map
import Data.Map (Map)

-- | >>> :main
-- 778
-- 1925
main :: IO ()
main =
 do input <- getInputArray 2024 10
    let paths = [pathsFrom input start | (start, '0') <- assocs input]
    print (sum (map length paths))
    print (sum (map sum    paths))

-- | Find the list of coordinates of peaks reachable from each trail
-- given a starting location.
-- 
-- Returns a map where keys are coordinates of peaks and values are the
-- number of distinct trails that reach each peak.
pathsFrom ::
    UArray Coord Char {- ^ topographical map                     -} ->
    Coord             {- ^ starting location                     -} ->
    Map Coord Int     {- ^ peak locations and trail multiplicity -}
pathsFrom input start = foldl (step input) (Map.singleton start 1) ['1'..'9']

-- | Given a map of current locations and multiplicity take a single step to
-- locations with the target height. Each new location inherits the
-- multiplicity of the paths that reached it.
step ::
    UArray Coord Char {- ^ topographical map                        -} ->
    Map Coord Int     {- ^ current locations and trail multiplicity -} ->
    Char              {- ^ target height                            -} ->
    Map Coord Int     {- ^ target locations and trail multiplicity  -}
step a m h = Map.fromListWith (+) [(j, n) | (i, n) <- Map.assocs m, j <- cardinal i, hj <- arrIx a j, hj == h]
