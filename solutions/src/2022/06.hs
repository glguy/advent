{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/6>

-}
module Main where

import Data.List (findIndex, tails)
import Data.Set qualified as Set

import Advent (format)

-- |
-- >>> :main
-- Just 1909
-- Just 3380
main :: IO ()
main =
 do input <- [format|2022 6 %s%n|]
    print (solve  4 input)
    print (solve 14 input)

solve :: Ord a => Int -> [a] -> Maybe Int
solve n input = fmap (n+) (findIndex (start n) (tails input))

start :: Ord a => Int -> [a] -> Bool
start n xs = length (Set.fromList (take n xs)) == n
