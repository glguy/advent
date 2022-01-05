{-# Language QuasiQuotes, ParallelListComp #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/1>

Count the number of increasing pairs of measurements.

-}
module Main where

import Advent (countBy, format)

-- | >>> :main
-- 1681
-- 1704
main :: IO ()
main =
 do input <- [format|2021 1 (%u%n)*|]
    print (solve 1 input)
    print (solve 3 input)

-- | >>> solve 1 [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
-- 7
--
-- >>> solve 3 [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
-- 5
solve ::
  Int {- ^ window size -} ->
  [Int] {- ^ measurements -} ->
  Int {- ^ count of ascending pairs -}
solve n input = countBy id [x < y | x <- input | y <- drop n input]
