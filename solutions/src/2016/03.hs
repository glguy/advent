{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/3>

-}
module Main where

import Advent
import Data.List

main :: IO ()
main =
  do input <- [format|3 (( *%d)*%n)*|]
     print (countBy goodTriangle input)
     print (countBy goodTriangle (rearrange input))

rearrange :: [[a]] -> [[a]]
rearrange = chunks 3 . concat . transpose

goodTriangle :: [Int] -> Bool
goodTriangle xs = x + y > z
  where
    [x,y,z] = sort xs
