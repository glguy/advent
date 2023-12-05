{-# Language QuasiQuotes, DataKinds, GADTs #-}
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

import Advent.Box ( intersectBox, subtractBox, Box', Box(..) )

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

smallestDestination :: [[(Interval, Int)]] -> [Interval] -> Int
smallestDestination maps = lowerBound . minimum . applyMaps maps

-- Verify that all the maps are presented in order
-- This is technically unnecessary for the given inputs, but it feels bad to
-- assume the order is right. Transform the (destination, source, length)
-- parameters to a source interval and shift value.
checkMaps :: [(String, String, [(Int, Int, Int)])] -> [[(Interval, Int)]]
checkMaps input = foldr processMap finish input "seed"
  where
    processMap (from, to, entries) continue expect =
      check expect from (map entryToInterval entries : continue to)

    finish final = check "location" final []

    entryToInterval (dst, src, len) = (interval src len, dst - src)

    check expected got x
      | expected == got = x
      | otherwise = error ("got " ++ got ++ " expected " ++ expected)

applyMaps :: [[(Interval, Int)]] -> [Interval] -> [Interval]
applyMaps maps xs =
  foldl (\acc ranges -> concatMap (applyMap ranges) acc) xs maps

applyMap :: [(Interval, Int)] -> Interval -> [Interval]
applyMap = foldr applyEntry pure
  where
    applyEntry (src, delta) continue box =
      case intersectBox src box of
        Nothing -> continue box
        Just i  ->
          shiftInterval delta i :
          concatMap continue (subtractBox src box)

-- Interval specialization of the Box module

-- | A one-dimensional cuboid
type Interval = Box' 1

-- | Construct an interval from a starting point and positive length
interval :: Int {- ^ start -} -> Int {- ^ length -} -> Interval
interval start len = Dim start (start + len) Pt

-- | Modify the lower and upper bounds of an interval by a fixed amount.
shiftInterval :: Int -> Interval -> Interval
shiftInterval delta (Dim lo hi Pt) = Dim (lo + delta) (hi + delta) Pt

-- | Retrieve the lower bound of an interval
lowerBound :: Interval -> Int
lowerBound (Dim x _ Pt) = x
