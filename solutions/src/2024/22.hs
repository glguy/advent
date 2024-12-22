{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/22>

>>> :{
:main + "1
2
3
4
2024
"
:}
37990510
23

-}
module Main (main) where

import Advent (format, times)
import Data.Bits (xor)
import Data.List (tails)
import Data.Map (Map)
import Data.Map.Strict qualified as Map

-- | >>> :main
-- 19241711734
-- 2058
main :: IO ()
main =
 do input <- [format|2024 22 (%u%n)*|]
    print (sum (map (times 2000 next) input))
    print (maximum (Map.unionsWith (+) (map characterize input)))

-- | Generate the next secret number
--
-- >>> map next [1, 10, 100, 2024]
-- [8685429, 4700978, 15273692, 8667524]
next :: Int -> Int
next x = c
  where
    prune y = y `mod` 16777216
    a = prune (x `xor` (x * 64))
    b = prune (a `xor` (a `div` 32))
    c = prune (b `xor` (b * 2048))

-- | >>> differences [1,3,4,10]
-- [2,1,6]
differences :: [Int] -> [Int]
differences xs = zipWith subtract xs (drop 1 xs)

characterize :: Int -> Map [Int] Int
characterize x = foldl add Map.empty candidates
  where
    xs = take 2001 (map (`mod` 10) (iterate next x))
    candidates = zip (map (take 4) (tails (differences xs))) (drop 4 xs)
    add m (k,v)
      | Map.member k m = m
      | otherwise      = Map.insert k v m
