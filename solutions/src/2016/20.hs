{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/20>

-}
module Main where

import Advent.Format (format)
import Data.List (sort)

type Blacklist = [(Integer,Integer)]

-- | >>> :main
-- 23923783
-- 125
main :: IO ()
main =
  do blacklist <- removeOverlap <$> [format|2016 20 (%lu-%lu%n)*|]
     print (lowest     blacklist)
     print (countValid blacklist)

-- | Remove all redundancy from the blacklist and put it in sorted order.
removeOverlap :: Blacklist -> Blacklist
removeOverlap = go . sort
  where
    go ((lo1,hi1):(lo2,hi2):rest)
      | hi1 >= lo2-1 = go ((lo1, max hi1 hi2) : rest)
    go (x:xs) = x : go xs
    go [] = []

-- | Smallest address that isn't blacklisted
lowest :: Blacklist -> Integer
lowest ((0,hi):_) = hi+1
lowest ((lo,_):_) = lo-1
lowest _ = 0

-- | Number of addresses not blacklisted
countValid :: Blacklist -> Integer
countValid xs = 2^(32::Int) - sum [1+b-a | (a,b) <- xs]
