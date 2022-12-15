{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/6>

>>> :main + "mjqjpqmgbljsphdztnvjfqwrcgsmlb\n"
7
19
>>> :main + "bvwbjplbgvbhsrlpgdmjqwftvncz\n"
5
23
>>> :main + "nppdvjthqldpwncqszvftbrmjlhg\n"
6
23
>>> :main + "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg\n"
10
29
>>> :main + "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw\n"
11
26

-}
module Main where

import Data.List (findIndex, tails)
import Data.Set qualified as Set

import Advent (format)

-- |
-- >>> :main
-- 1909
-- 3380
main :: IO ()
main =
 do input <- [format|2022 6 %s%n|]
    print (solve  4 input)
    print (solve 14 input)

solve :: Ord a => Int -> [a] -> Int
solve n input = maybe undefined (n+) (findIndex (start n) (tails input))

start :: Ord a => Int -> [a] -> Bool
start n xs = length (Set.fromList (take n xs)) == n
