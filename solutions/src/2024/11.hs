{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/11>

This solution runs efficiently by remembering how many of each stone
there are and blinking all of that same kind of stone all at once.
While the problem does state that order is preserved, the question
it asks about the stones does not depend on order, so we forget that
order!

>>> :main + "125 17\n"
55312
65601038650482

-}
module Main (main) where

import Advent (format, times)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | >>> :main
-- 202019
-- 239321955280205
main :: IO ()
main =
 do input <- [format|2024 11 %u& %n|]
    print (solve 25 input)
    print (solve 75 input)

-- | Compute the number of stones resulting from a starting set of stones
-- and a number of blink iterations.
solve :: Int -> [Int] -> Int
solve n input = sum (times n blinks (Map.fromListWith (+) [(i, 1) | i <- input]))

-- | Blink all the stones at once. Stone numbers are mapped to multiplicity.
blinks :: Map Int Int -> Map Int Int
blinks stones = Map.fromListWith (+) [(stone', n) | (stone, n) <- Map.assocs stones, stone' <- blink stone]

-- | Blink a single stone and figure out what stones it turns into.
blink :: Int -> [Int]
blink 0 = [1]         -- 0 -> 1
blink n               -- split in half if even length
  | (w, 0) <- length (show n) `quotRem` 2
  , (l, r) <- n `quotRem` (10 ^ w)
  = [l, r]
blink n = [n * 2024]  -- otherwise multiply by 2024
