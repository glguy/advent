{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/7>

Find the minimum fuel cost to move the submarines to a common point.

-}
module Main (main) where

import Advent (format)
import Data.List (sort)

-- | >>> :main
-- 336721
-- 91638945
main :: IO ()
main =
 do inp <- [format|7 %u&,%n|]

    let median = sort inp !! (length inp `div` 2)
    print (sum [abs (x - median) | x <- inp])

    let mean = sum inp `div` length inp
    print (minimum [sum [triangle (abs (x-a)) | x <- inp] | a <- [mean, mean+1]])

-- | Sum of numbers from 1 to @n@
triangle :: Int -> Int
triangle n = n * (n+1) `div` 2
