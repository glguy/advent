{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/1>

Compute fuel costs for a rocket.

-}
module Main (main) where

import Advent.Format (format)

-- | >>> :main
-- 3188480
-- 4779847
main :: IO ()
main =
  do inp <- [format|2019 1 (%lu%n)*|]
     print (sum (map fuelCost          inp))
     print (sum (map recursiveFuelCost inp))

-- | Compute fuel cost given a mass.
--
-- >>> fuelCost 12
-- 2
-- >>> fuelCost 14
-- 2
-- >>> fuelCost 1969
-- 654
-- >>> fuelCost 100756
-- 33583
fuelCost :: Integer -> Integer
fuelCost x = x `div` 3 - 2

-- | Compute fuel cost given a mass.
--
-- >>> recursiveFuelCost 14
-- 2
-- >>> recursiveFuelCost 1969
-- 966
-- >>> recursiveFuelCost 100756
-- 50346
recursiveFuelCost :: Integer -> Integer
recursiveFuelCost = sum . takeWhile (> 0) . tail . iterate fuelCost
