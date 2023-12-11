{-# Language NumericUnderscores, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/11>

We're given a map of galaxies and need to find the
sum of all distances between the pairs of galaxies.
The twist is that empty rows and columns are expanded.

We can simplify this problem by solving the X and Y
axes separately. We can collapse the input down to
a count of how many galaxies exist at each location
on a single axis.

>>> :{
:main +
"...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"
:}
374
82000210

-}
module Main where

import Advent (getInputLines, counts)
import Advent.Coord (Coord(C), coordLines)
import Data.Map qualified as Map

-- |
--
-- >>> :main
-- 9965032
-- 550358864332
main :: IO ()
main =
 do input <- getInputLines 2023 11
    let (rows, cols) = unzip [(y, x) | (C y x, '#') <- coordLines input]
        solve n      = solve1 n rows + solve1 n cols
    print (solve 2)
    print (solve 1_000_000)

-- | Solve the travel problem along a single axis
solve1 ::
  Int   {- ^ expansion factor        -} ->
  [Int] {- ^ galaxy positions        -} ->
  Int   {- ^ total distance traveled -}
solve1 ex positions =
  case Map.assocs counted of
    []             -> 0
    (here, n) : xs -> go xs n here 0
  where
    counted = counts positions
    total   = sum counted

    go []                _ _    acc = acc
    go ((there, m) : xs) n here acc = go xs (n + m) there acc'
      where
        crossings = n * (total - n)
        distance  = (there - here - 1) * ex + 1
        acc'      = acc + crossings * distance
