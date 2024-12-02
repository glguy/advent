{-# Language QuasiQuotes, RecordWildCards #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2015
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/14>

>>> :{
:main +
  "time 1000\n\
  \Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.\n\
  \Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.\n"
:}
1120
689

-}
module Main where

import Advent (format, partialSums)
import Data.List (transpose)
import Data.Maybe (fromMaybe)

data Reindeer = Reindeer
  { speed     :: Int   -- ^ units of distance flown per second
  , stamina   :: Int   -- ^ number of seconds flown before rest
  , breaktime :: Int   -- ^ number of seconds rested before flying
  }

-- |
-- >>> :main
-- 2660
-- 1256
main :: IO ()
main =
 do (mbtime,input) <- [format|2015 14 (time %u%n|)(%s can fly %u km/s for %u seconds, but then must rest for %u seconds.%n)*|]
    let time = fromMaybe 2503 mbtime
    let rs = [Reindeer{..} | (_, speed, stamina, breaktime) <- input]    
    let race = map (take time . positions) rs
    print (maximum (map last race))
    print (maximum (scores race))

-- | Compute the position of each reindeer at each second of the race
positions :: Reindeer -> [Int]
positions r
  = drop 1
  $ partialSums
  $ cycle
  $ replicate (stamina   r) (speed r) ++ replicate (breaktime r) 0

-- | Given a list of race positions return a list of scores
scores :: [[Int]] -> [Int]
scores = map sum . transpose . map awardPoints . transpose

-- | Map each position to 1 point if it's in the lead or 0 otherwise
awardPoints ::
  [Int] {- ^ positions -} ->
  [Int] {- ^ points    -}
awardPoints posns = [ if p == best then 1 else 0 | p <- posns ]
  where
  best = maximum posns
