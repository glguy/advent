{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/7>

>>> :{
:main + "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"
:}
3749
11387

-}
module Main (main) where

import Advent (format)

-- | >>> :main
-- 6231007345478
-- 333027885676693
main :: IO ()
main =
 do input <- [format|2024 7 (%u: %u& %n)*|]
    print (sum [x | (x, y) <- input, isValid [(+), (*)]      x y])
    print (sum [x | (x, y) <- input, isValid [(+), (*), cat] x y])

isValid :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
isValid _ _ [] = False
isValid _ x [y] = x == y
isValid ops x (y:z:w) =
    any (\op -> let yz = op y z in x >= yz && isValid ops x (yz:w)) ops

-- | The concatenation operator (||) combines the digits from its left
-- and right inputs into a single number.
cat :: Int -> Int -> Int
cat x y = read (show x ++ show y)
