{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/1>

Count open and closing parentheses.

>>> :main + "(())\n"
0

>>> :main + "()()\n"
0

>>> :main + "(((\n"
3

>>> :main + "(()(()(\n"
3

>>> :main + "))(((((\n"
3
1

>>> :main + "())\n"
-1
3

>>> :main + "))(\n"
-1
1

>>> :main + ")))\n"
-3
1

>>> :main + ")())())\n"
-3
1

>>> :main + "()())\n"
-1
5

-}
module Main where

import Advent (format)
import Data.Foldable (traverse_)
import Data.List (elemIndex, scanl')

-- | >>> :main
-- 138
-- 1771
main :: IO ()
main =
 do inp <- [format|2015 1 %s%n|]
    let xs = map interpret inp
    print (sum xs)
    traverse_ print (part2 xs)

interpret :: Char -> Int
interpret '(' = 1
interpret ')' = -1
interpret x   = error ("No interpretation for: " ++ [x])

part2 :: [Int] -> Maybe Int
part2 = elemIndex (-1) . partialSums

partialSums :: Num a => [a] -> [a]
partialSums = scanl' (+) 0
