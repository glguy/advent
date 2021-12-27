{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/1>

Count open and closing parentheses.

-}
module Main where

import Advent (getInputLines)
import Data.Foldable (traverse_)
import Data.List (findIndex, scanl')

-- | >>> :main
-- 138
-- 1771
main :: IO ()
main =
 do [inp] <- getInputLines 1
    let xs = map interpret inp
    print (sum xs)
    traverse_ print (part2 xs)

interpret :: Char -> Int
interpret '(' = 1
interpret ')' = -1

part2 :: [Int] -> Maybe Int
part2 = findIndex (< 0) . partialSums

partialSums :: Num a => [a] -> [a]
partialSums = scanl' (+) 0
