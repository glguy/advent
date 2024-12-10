{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/10>

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
module Main where

import Advent (arrIx, getInputArray)
import Advent.Coord (Coord, cardinal)
import Advent.Search (dfs)
import Data.Array.Unboxed (UArray, (!), assocs)

-- | >>> :main
-- 778
-- 1925
main :: IO ()
main =
 do input <- getInputArray 2024 10
    print (part1 input)
    print (part2 input)

part1 :: UArray Coord Char -> Int
part1 a = length [() | (start, '0') <- assocs a, end <- dfs step start, a!end == '9']
  where
    step x = [y | y <- cardinal x, h <- arrIx a y, succ (a ! x) == h]

part2 :: UArray Coord Char -> Int
part2 a = length [() | (start, '0') <- assocs a, (end, _) <- dfs step (start, []), a!end == '9']
  where
    step (x, xs) = [(y, x:xs) | y <- cardinal x, h <- arrIx a y, succ (a!x) == h]
