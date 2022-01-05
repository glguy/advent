{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/3>

Determine triples that are valid triangle side lengths.

-}
module Main where

import Advent (format, chunks, countBy)
import Data.List (sort, transpose)

-- | >>> :main
-- 1050
-- 1921
main :: IO ()
main =
 do input <- [format|2016 3 (( *%d)*%n)*|]
    print (countBy goodTriangle input)
    print (countBy goodTriangle (rearrange input))

rearrange :: [[a]] -> [[a]]
rearrange = chunks 3 . concat . transpose

goodTriangle :: [Int] -> Bool
goodTriangle xs = x + y > z
  where
    [x,y,z] = sort xs
