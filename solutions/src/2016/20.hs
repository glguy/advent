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

main :: IO ()
main =
  do blacklist <- processBlacklist <$> [format|2016 20 (%lu-%lu%n)*|]
     print (lowest     blacklist)
     print (countValid blacklist)

-- | Remove overlaps and ensure that the blacklist surrounds
-- the whole address space.
processBlacklist :: Blacklist -> Blacklist
processBlacklist xs =
  removeOverlap (sort ([(-1,-1),(2^32,2^32)] ++ xs))

lowest :: Blacklist -> Integer
lowest ((_,hi):_) = hi+1
lowest _          = 0

removeOverlap :: Blacklist -> Blacklist
removeOverlap ((lo1,hi1):(lo2,hi2):rest)
  | hi1 >= lo2-1 = removeOverlap ((lo1, max hi1 hi2) : rest)
removeOverlap (x:xs) = x : removeOverlap xs
removeOverlap [] = []

countValid :: Blacklist -> Integer
countValid xs = sum (zipWith gapSize xs (tail xs))
  where
    gapSize (_,hi1) (lo2,_) = lo2 - hi1 - 1
