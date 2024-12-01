{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/1>

Find sum of pairwise distances between two sorted lists
and find sum of elements in first list multiplied by
frequences of that element in the second list.

>>> :{
:main +
"3   4
4   3
2   5
1   3
3   9
3   3
"
:}
11
31

-}
module Main (main) where

import Advent (counts, format)
import Data.List (sort)
import Data.Map qualified as Map

-- | >>> :main
-- 1530215
-- 26800609
main :: IO ()
main =
 do (left, right) <- unzip <$> [format|2024 1 (%u   %u%n)*|]
    print (sum (zipWith distance (sort left) (sort right)))
    
    let tab = counts right
    print (sum [x * Map.findWithDefault 0 x tab | x <- left])

-- | Absolute distance between two numbers.
distance :: Int -> Int -> Int
distance x y = abs (x - y)
