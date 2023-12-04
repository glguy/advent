{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/4>

We get a list of the winning numbers and out numbers
and have to figure out how many points we got and then
using a recursive card winning system count up how many
cards we played.

>>> :{
:main +
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"
:}
13
30

-}
module Main where

import Advent (format)
import Data.List (intersect)

-- | Parse the input and print out the answers to both parts.
--
-- >>> :main
-- 21485
-- 11024379
main :: IO ()
main =
 do input <- [format|2023 4 (Card +%d:( +%d)* %|( +%d)*%n)*|]
    let wins = [length (a `intersect` b) | (_, a, b) <- input]
    print (sum (map points wins))
    print (sum (asPart2 wins))

-- | Convert wins to points for part 1
points :: Int -> Int
points 0 = 0
points n = 2 ^ (n - 1)

-- | Convert a list of wins for each card into the number of cards
-- each card turns into.
asPart2 :: [Int] -> [Int]
asPart2 = foldr (\wins xs -> 1 + sum (take wins xs) : xs) []
