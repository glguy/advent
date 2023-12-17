{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/17>

>>> :{
:main +
"2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
" 
:}
102
94

>>> :{
:main +
"111111111111
999999999991
999999999991
999999999991
999999999991
"
:}
59
71

-}
module Main where

import Advent (getInputArray, arrIx)
import Advent.Coord (east, south, turnLeft, turnRight, Coord)
import Advent.Search (astarOnN, AStep(..))
import Data.Array.Unboxed (amap, bounds, UArray)
import Data.Char (digitToInt)

-- | Parse input grid and print both answer parts.
--
-- >>> :main
-- 866
-- 1010
main :: IO ()
main =
 do input <- amap digitToInt <$> getInputArray 2023 17
    print (solve 0  3 input)
    print (solve 4 10 input)

solve :: Int -> Int -> UArray Coord Int -> Int
solve lo hi input =
  head [cost | ((loc, _, dist), cost) <- astarOnN id (step lo hi input) begin, loc == end, dist >= lo]
  where
    (start, end) = bounds input
    begin = [(start, east, 0),(start, south, 0)]

type S = (Coord, Coord, Int) -- location, direction, distance in direction

step :: Int -> Int -> UArray Coord Int -> S -> [AStep S]
step lo hi input (here, dir, fuel) =
  [ AStep {
      astepCost      = val,
      astepHeuristic = 0,
      astepNext      = (next, dir', fuel')
    }
  | dir' <- if fuel < lo then [dir] else [dir, turnLeft dir, turnRight dir]
  , let next = here + dir'
  , val <- arrIx input next
  , let fuel' = if dir == dir' then fuel + 1 else 1
  , fuel' <= hi
  ]