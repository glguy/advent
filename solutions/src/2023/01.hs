{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/1>

-}
module Main where

import Data.List (isPrefixOf, tails)
import Data.Maybe (listToMaybe, mapMaybe)

import Advent (format)

part1 :: [(String, Int)]
part1 = [(show i, i) | i <- [0..9]]

part2 :: [(String, Int)]
part2 = part1 ++
   [("one"  ,1),("two"  ,2),("three",3),
    ("four" ,4),("five" ,5),("six"  ,6),
    ("seven",7),("eight",8),("nine" ,9)]

decode :: [(String, Int)] -> String -> Int
decode mapping str = 10 * head ns + last ns
  where
    f :: String -> Maybe Int
    f x = listToMaybe [v | (k,v) <- mapping, k `isPrefixOf` x]
    ns = mapMaybe f (tails str)

main :: IO ()
main =
 do input <- [format|2023 1 (%s%n)*|]
    print (sum (map (decode part1) input))
    print (sum (map (decode part2) input))
