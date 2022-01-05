{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/11>

Simulating a sea of octopuses that flash when they get excited.

-}
module Main (main) where

import Advent (count, getInputMap)
import Advent.Coord (Coord(..), neighbors)
import Data.Char (digitToInt)
import Data.List (elemIndex, foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

-- | >>> :main
-- 1585
-- 382
main :: IO ()
main =
 do inp <- fmap digitToInt <$> getInputMap 2021 11
    let flashes = simulate inp
    print (sum (take 100 flashes))
    print (1 + fromJust (elemIndex (Map.size inp) flashes))

-- | Initial grid state to flashes per step
simulate :: Map Coord Int -> [Int]
simulate = map (count 0) . tail . iterate step

-- | Advance the state of the world one time step
step :: Map Coord Int -> Map Coord Int
step m = foldl' excite (fmap (1 +) m) [k | (k, 9) <- Map.toList m]

-- | Excite an octopus at the given location
excite :: Map Coord Int -> Coord -> Map Coord Int
excite m x =
  case Map.lookup x m of
    Just e
      | e >= 9 -> foldl' excite (Map.insert x 0 m) (neighbors x)
      | e >= 1 -> Map.insert x (1 + e) m
    _          -> m
