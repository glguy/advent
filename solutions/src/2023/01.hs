{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/1>

For each line in the input, create a two digit number from the first and
last digits on the line, then sum up all of these numbers.

>>> decode part1 <$> ["1abc2","pqr3stu8vwx","a1b2c3d4e5f","treb7uchet"]
[12,38,15,77]

>>> decode part2 <$> ["two1nine","eightwothree","abcone2threexyz","xtwone3four","4nineeightseven2","zoneight234","7pqrstsixteen"]
[29,83,13,24,42,14,76]

-}
module Main where

import Data.List (isPrefixOf, tails)

import Advent (fromDigits, format)

-- |
--
-- >>> :main
-- 55123
-- 55260
main :: IO ()
main =
 do input <- [format|2023 1 (%s%n)*|]
    print (sum (map (decode part1) input))
    print (sum (map (decode part2) input))

part1 :: [(String, Int)]
part1 = [(show i, i) | i <- [0..9]]

part2 :: [(String, Int)]
part2 = part1 ++
   [("one"  ,1),("two"  ,2),("three",3),
    ("four" ,4),("five" ,5),("six"  ,6),
    ("seven",7),("eight",8),("nine" ,9)]

earliest :: Eq k => [([k], a)] -> [k] -> a
earliest entries haystack =
  head [v | needle <- tails haystack, (k,v) <- entries, k `isPrefixOf` needle]

decode :: [(String, Int)] -> String -> Int
decode mapping str = fromDigits 10 [d1,d2]
  where
    d1 = earliest mapping str
    d2 = earliest mapping' (reverse str)
    mapping' = [(reverse k, v) | (k,v) <- mapping]
