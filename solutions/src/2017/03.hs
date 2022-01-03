{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/3>

Day 3 asks questions about Ulam's Spiral <https://en.wikipedia.org/wiki/Ulam_spiral>

-}
module Main where

import Advent (format)
import Advent.Coord
import Data.List (mapAccumL)
import Data.Maybe (mapMaybe)
import Data.Map qualified as Map

main :: IO ()
main =
  do n <- [format|3 %u%n|]
     print (part1 n)
     print (part2 n)


-- | Coordinates in the spiral order starting with the origin
--
-- >>> [C 0 0,C 1 0,C 1 1,C 0 1,C (-1) 1] `Data.List.isPrefixOf` coords
-- True
coords :: [Coord]
coords = scanl (+) origin movements
  where
    directions = [south, east, north, west]

    movements =
      do (d,n) <- cycle directions `zip` (replicate 2 =<< [1..])
         replicate n d


-- | Find manhattan distance of nth visited coordinate using 1-based counting
--
-- >>> part1 1
-- 0
-- >>> part1 12
-- 3
-- >>> part1 23
-- 2
-- >>> part1 1024
-- 31
part1 :: Int -> Int
part1 input = manhattan origin (coords !! (input-1))


-- | Infinite list of the writes done when populating the cells
-- in spiral order by using the sum of the earlier populated
-- neighbors.
--
-- >>> [1,1,2,4,5,10,11,23,25,26,54,57,59,122,133,142,147,304,330,351,362,747,806] `Data.List.isPrefixOf` part2writes
-- True
part2writes :: [Int]
part2writes = 1 : snd (mapAccumL go (Map.singleton origin 1) (tail coords))
  where
    go m c = (Map.insert c here m, here)
      where
        here = sum (mapMaybe (`Map.lookup` m) (neighbors c))

-- | Returns the first value written in part 2 of the problem that is larger
-- than the given input value.
part2 :: Int -> Int
part2 input = head (dropWhile (<= input) part2writes)
