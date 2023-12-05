{-# Language QuasiQuotes, DataKinds, GADTs #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/5>

>>> :{
:main +
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"
35
46

-}
module Main where

import Advent (format, chunks)

import Advent.Box ( intersectBox, subtractBox, Box(..) )
import Advent.Nat ( Nat(Z, S) )

-- |
--
-- >>> :main
-- 457535844
-- 41222968
main :: IO ()
main =
 do (seeds, maps) <- [format|2023 5 seeds:( %d)*%n(%n%s-to-%s map:%n(%d %d %d%n)*)*|]
    print (smallestDestination maps [rng start 1 | start     <-          seeds])
    print (smallestDestination maps [rng start n | [start,n] <- chunks 2 seeds])

smallestDestination :: [(String, String, [(Int, Int, Int)])] -> [Range] -> Int
smallestDestination maps = lowerBound . minimum . concatMap (convertSeeds maps)

-- assumes maps are in order
convertSeeds :: [(String, String, [(Int,Int,Int)])] -> Range -> [Range]
convertSeeds maps x =
  foldl (\acc (_from,_to,ranges) -> concatMap (applyRanges ranges) acc) [x] maps

type Range = Box ('S 'Z)

rng :: Int {- ^ start -} -> Int {- ^ length -} -> Range
rng s n = Dim s (s+n) Pt

lowerBound :: Range -> Int
lowerBound (Dim x _ Pt) = x

applyRanges :: [(Int, Int, Int)] -> Range -> [Range]
applyRanges = foldr applyRange pure
  where
    applyRange (dst, src, len) continue seeds =
      case intersectBox seeds (rng src len) of
        Nothing -> continue seeds
        Just (Dim lo hi Pt) ->
          rng (dst + lo - src) (hi - lo) :
          concatMap continue (subtractBox (rng src len) seeds)
