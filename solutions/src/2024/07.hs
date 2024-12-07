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
import Data.List (sort, isSuffixOf)
-- | >>> :main
-- 6231007345478
-- 333027885676693
main :: IO ()
main =
 do input <- [format|2024 7 (%u: %u& %n)*|]
    print (sum [x | (x, y) <- input, isValid1 x y])
    print (sum [x | (x, y) <- input, isValid2 x y])

isValid1 :: Int -> [Int] -> Bool
isValid1 x ys = 0 `elem` foldr f [x] ys
  where
    f a bs = [o | b <- bs, o <- [b `div` a | b `mod` a == 0] ++ [b - a | b >= a]]

isValid2 :: Int -> [Int] -> Bool
isValid2 x ys = 0 `elem` foldr f [x] ys
  where
    f :: Int -> [Int] -> [Int]
    f a bs = [o | b <- bs, o <- [b `div` a | b `mod` a == 0]
                             ++ [b - a | b >= a]
                             ++ [read (take (length (show b) - length (show a)) (show b)) | show a `isSuffixOf` show b]]