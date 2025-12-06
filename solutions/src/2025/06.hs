{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/6>

>>> :{
:main +
"123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
"
:}
4277556
3263827

-}
module Main where

import Advent (getInputLines)
import Data.List (transpose)
import Data.List.Split (splitWhen)

-- | >>> :main
-- 7229350537438
-- 11479269003550
main :: IO ()
main =
 do input <- getInputLines 2025 6
    let problems = splitWhen (all (' ' ==)) (transpose input)
    print (sum (map solve1 problems))
    print (sum (map solve2 problems))

solve1 :: [String] -> Int
solve1 xs = finish (head (last xs')) (init xs')
    where
      xs' = transpose xs

solve2 :: [String] -> Int
solve2 (x:xs) = finish (last x) (init x : xs)

finish :: Char -> [String] -> Int
finish '+' xs = sum (map read xs)
finish '*' xs = product (map read xs)
