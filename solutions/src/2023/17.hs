{-# Language QuasiQuotes #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/17>

Shortest-path graph search where the graph states are the triple of
a location, direction, and distance traveled in that direction.

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
import Advent.Search (astarN, AStep(..))
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
  head [cost | (S loc _ dist, cost) <- astarN (step lo hi input) begin
             , loc == end -- at the end
             , lo <= dist -- able to stop
             ]
  where
    (start, end) = bounds input
    begin = [S start east 0, S start south 0]

data S = S !Coord !Coord !Int -- ^ location, direction, distance
  deriving (Eq, Ord)

step :: Int -> Int -> UArray Coord Int -> S -> [AStep S]
step lo hi input (S here dir dist) =
  [ AStep {
      astepNext      = S here' dir' dist',
      astepCost      = cost,
      astepHeuristic = 0}
  | (dir', dist') <-
    [(dir, dist + 1) | dist < hi] ++
    if dist < lo then [] else [(turnLeft dir, 1), (turnRight dir, 1)]
  , let here' = here + dir'
  , cost <- arrIx input here'
  ]
