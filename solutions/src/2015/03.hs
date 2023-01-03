{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/3>

Follow up, down, left, right instructions to build a path.

>>> :main + ">\n"
2
2

>>> :main + "^v\n"
2
3

>>> :main + "^>v<\n"
4
3

>>> :main + "^v^v^v^v^v\n"
2
11

-}
module Main where

import Data.List (transpose)
import Data.Maybe (mapMaybe)

import Advent (chunks, counts, format, partialSums)
import Advent.Coord (Coord, origin, north, east, south, west, charToVec)

-- | >>> :main
-- 2572
-- 2631
main :: IO ()
main =
 do input <- [format|2015 3 (^|v|<|>)*!%n|]
    let directions = mapMaybe charToVec input
    print (countHouses 1 directions)
    print (countHouses 2 directions)

countHouses :: Int {- ^ workers -} -> [Coord] -> Int
countHouses n =
  length . counts . concatMap partialSums . transpose . chunks n
