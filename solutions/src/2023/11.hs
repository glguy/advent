{-# Language QuasiQuotes, NumericUnderscores, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/11>

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
import Advent.Coord (coordLines, Coord(C))
import Data.List (inits, tails)
import Data.Map (Map)
import Data.Map qualified as Map

-- |
--
-- >>> :main
-- 9965032
-- 550358864332
main :: IO ()
main =
 do input <- getInputLines 2023 11
    let galaxies = [c | (c,'#') <- coordLines input]
        rows     = counts [y | C y _ <- galaxies]
        cols     = counts [x | C _ x <- galaxies]
        solve n  = solve1 n rows + solve1 n cols
    print (solve 2)
    print (solve 1_000_000)

-- | Solve the travel problem along a single axis
solve1 ::
  Int         {- ^ expansion factor                        -} ->
  Map Int Int {- ^ map from location to number of galaxies -} ->
  Int         {- ^ total distance traveled                 -}
solve1 ex m =
  let total = sum m in
  case Map.assocs m of
    [] -> 0
    (here,n):xs -> solve1' ex n (total - n) 0 here xs

solve1' ::
  Int         {- ^ expandsion factor              -} ->
  Int         {- ^ galaxies to the left           -} ->
  Int         {- ^ galaxies to the right          -} ->
  Int         {- ^ accumulator                    -} ->
  Int         {- ^ current location               -} ->
  [(Int,Int)] {- ^ remaining locations and counts -} ->
  Int         {- ^ total distances traveled       -}
solve1' ex _ _ acc _ [] = acc
solve1' ex l r acc here ((there,m):xs) =
  solve1' ex (l+m) (r-m) (acc + crossings * distance) there xs
  where
    crossings = l * r
    distance = (there - here - 1) * ex + 1