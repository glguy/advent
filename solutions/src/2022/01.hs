{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/1>

>>> :main + "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n"
24000
45000

-}
module Main where

import Advent (format)
import Data.List (sortBy)

-- |
-- >>> :main
-- 67658
-- 200158
main :: IO ()
main =
 do input <- [format|2022 1 (%u%n)*&%n|]
    let elves = sortBy (flip compare) (map sum input)
    let top n = sum (take n elves)
    print (top 1)
    print (top 3)
