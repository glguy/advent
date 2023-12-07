{-# Language QuasiQuotes, DataKinds, GADTs, ImportQualifiedPost, MonadComprehensions #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/5>

Given many layers of linear shifts of intervals we need
to efficiently apply those shifts to a number of input
ranges and find the lowest bound of the output intervals.

>>> :{
:main +
"seeds: 79 14 55 13
\nseed-to-soil map:
50 98 2
52 50 48
\nsoil-to-fertilizer map:
0 15 37
37 52 2
39 0 15
\nfertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4
\nwater-to-light map:
88 18 7
18 25 70
\nlight-to-temperature map:
45 77 23
81 45 19
68 64 13
\ntemperature-to-humidity map:
0 69 1
1 0 69
\nhumidity-to-location map:
60 56 37
56 93 4
"
:}
35
46

-}
module Main where

import Advent (format, chunks)
import Advent.Box (intersectBox, Box', Box(..), subtractBox')
import Control.Exception (assert)
import Control.Monad (foldM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
-- |
--
-- >>> :main
-- 457535844
-- 41222968
main :: IO ()
main =
 do (seeds, rawMaps) <- [format|2023 5 seeds:( %d)*%n(%n%s-to-%s map:%n(%d %d %d%n)*)*|]
    let maps = checkMaps rawMaps
    print (smallestDestination maps [interval start 1 | start     <-          seeds])
    print (smallestDestination maps [interval start n | [start,n] <- chunks 2 seeds])

-- | Map from lower-bound to (upper-bound, shift amount)
type IntervalRewriter = Map Int (Int, Int)

-- | Apply all the maps to all the intervals and return the smallest output
smallestDestination :: [IntervalRewriter] -> [Interval] -> Int
smallestDestination maps = lowerBound . minimum . concatMap (applyMaps maps)

-- Verify that all the maps are presented in order.
-- Construct an interval rewriter from each sublist of entries
checkMaps :: [(String, String, [(Int, Int, Int)])] -> [IntervalRewriter]
checkMaps input = assert (froms ++ ["location"] == "seed" : tos) (map toRewriter maps)
  where
    (froms, tos, maps) = unzip3 input
    toRewriter xs = Map.fromList [(src,  (src + len, dst - src)) | (dst, src, len) <- xs]

-- | Apply the rewrite maps left to right to the input interval.
applyMaps :: [IntervalRewriter] -> Interval -> [Interval]
applyMaps = flip (foldM (flip applyMap))

-- | Apply a single rewrite map to an input interval.
applyMap :: IntervalRewriter -> Interval -> [Interval]
applyMap intervals x =
  fromMaybe [x]
  [ shiftInterval d overlap
  : concatMap (applyMap intervals) (subtractBox' overlap x)
  | (lo, (hi, d)) <- Map.lookupLT (upperBound x) intervals
  , overlap       <- intersectBox (Dim lo hi Pt) x
  ]

-- Interval specialization of the Box module

-- | A one-dimensional cuboid
type Interval = Box' 1

-- | Construct an interval from a starting point and positive length
interval :: Int {- ^ start -} -> Int {- ^ length -} -> Interval
interval start len = Dim start (start + len) Pt

-- | Modify the lower and upper bounds of an interval by a fixed amount.
shiftInterval :: Int -> Interval -> Interval
shiftInterval delta (Dim lo hi Pt) = Dim (lo + delta) (hi + delta) Pt

-- | Retrieve the inclusive lower-bound of an interval
lowerBound :: Interval -> Int
lowerBound (Dim x _ Pt) = x

-- | Retrieve the exclusive upper-bound of an interval
upperBound :: Interval -> Int
upperBound (Dim _ x Pt) = x
