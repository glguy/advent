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
    print (smallestDestination maps [interval start 1 | start     <-          seeds])
    print (smallestDestination maps [interval start n | [start,n] <- chunks 2 seeds])

smallestDestination :: [(String, String, [(Int, Int, Int)])] -> [Interval] -> Int
smallestDestination maps = lowerBound . minimum . concatMap (convertSeeds maps)

-- assumes maps are in order
convertSeeds :: [(String, String, [(Int,Int,Int)])] -> Interval -> [Interval]
convertSeeds maps x =
  foldl (\acc (_from,_to,ranges) -> concatMap (applyRewrites ranges) acc) [x] maps

type Interval = Box ('S 'Z)

interval :: Int {- ^ start -} -> Int {- ^ length -} -> Interval
interval s n = Dim s (s+n) Pt

lowerBound :: Interval -> Int
lowerBound (Dim x _ Pt) = x

applyRewrites :: [(Int, Int, Int)] -> Interval -> [Interval]
applyRewrites = foldr applyRewrite pure
  where
    applyRewrite (dst, src, len) continue seeds =
      case intersectBox seeds (interval src len) of
        Nothing -> continue seeds
        Just (Dim lo hi Pt) ->
          interval (dst + lo - src) (hi - lo) :
          concatMap continue (subtractBox (interval src len) seeds)
