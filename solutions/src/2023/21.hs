{-# Language QuasiQuotes, TemplateHaskell, BangPatterns, BlockArguments, LambdaCase, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/21>

The input cases are real super special for part 2,
don't bother thinking about them too hard, just
extrapolate and check.

-}
module Main (main) where

import Advent (getInputArray, times, ordNub, arrIx)
import Advent.Coord (Coord(..), cardinal)
import Data.Array.Unboxed (UArray, bounds, assocs, amap)

main :: IO ()
main =
 do input <- getInputArray 2023 21
    let start = head [ start | (start, 'S') <- assocs input]
    let input' = amap (\x -> x == 'S' || x == '.') input :: UArray Coord Bool

    print $ length $ times 64 (ordNub . concatMap (step input')) [start]

    let t0 = length $ times (65+131*0) (ordNub . concatMap (step input')) [start]
    let t1 = length $ times (65+131*1) (ordNub . concatMap (step input')) [start]
    let t2 = length $ times (65+131*2) (ordNub . concatMap (step input')) [start]

    let d01 = t1 - t0
    let d12 = t2 - t1
    let d01_12 = d12 - d01

    let f x = t0 + (t1-t0) * x + x * (x-1) `quot` 2 * d01_12
    print (f (26501365 `quot` 131))

step :: UArray Coord Bool -> Coord -> [Coord]
step input here =
  [ here'
  | here' <- cardinal here
  , let (_, C ymax xmax) = bounds input
  , let modIt (C y x) = C (y `mod` (ymax+1)) (x `mod` (xmax+1))
  , let hereLogical = modIt here'
  , True <- arrIx input hereLogical
  ]
