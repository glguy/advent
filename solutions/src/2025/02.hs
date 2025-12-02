{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : 02 02 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/02/02>

>>> :main + "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124\n"
1227775554
4174379265

-}
module Main (main) where

import Advent (format, same, chunks)

-- | >>> :main
-- 28146997880
-- 40028128307
main :: IO ()
main =
    do
    input <- [format|2025 02 (%u-%u)&,%n|]
    let numbers = do (lo, hi) <- input; [lo .. hi]
    print (sum (filter part1 numbers))
    print (sum (filter part2 numbers))

-- | Predicate for numbers that are made of the same
-- sequence of digits repeated twice.
part1 :: Int -> Bool
part1 x = even n && a == b
  where
    str    = show x
    n      = length str
    (a, b) = splitAt (n `quot` 2) str

-- | Predicate for numbers that are made of the same
-- sequence of digits repeated at least twice.
part2 :: Int -> Bool
part2 x = or [same (chunks i str) | i <- [1 .. n `quot` 2], n `mod` i == 0]
  where
    str = show x
    n   = length str
