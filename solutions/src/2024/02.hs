{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/2>

Find the number of reports that are ascending or descending
and have neighbor differences between 1 and 3.

>>> :{
:main +
"7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
"
:}
2
4

-}
module Main (main) where

import Advent (countBy, format)
import Data.List (inits, tails)

-- | >>> :main
-- 631
-- 665
main :: IO ()
main =
 do input <- [format|2024 2 (%u& %n)*|]
    print (countBy isSafe input)
    print (countBy (\x -> any isSafe (x : removeOne x)) input)

isSafe :: [Int] -> Bool
isSafe xs = all p1 ds || all p2 ds
    where
        p1 x = -3 <= x && x <= -1
        p2 x = 1 <= x && x <= 3
        ds = zipWith (-) xs (drop 1 xs)

removeOne :: [a] -> [[a]]
removeOne xs = zipWith (++) (inits xs) (drop 1 (tails xs))
