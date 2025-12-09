{-# Language QuasiQuotes, GADTs, DataKinds #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/5>

>>> :{
:main +
"3-5
10-14
16-20
12-18
\&
1
5
8
11
17
32
"
:}
3
14

-}
module Main where

import Advent (countBy, format)
import Advent.Box (size, unionBoxes, Box', Box(Pt, Dim))

-- | >>> :main
-- 664
-- 350780324308385
main :: IO ()
main =
 do (fresh, input) <- [format|2025 5 (%u-%u%n)*%n(%u%n)*|]

    -- NB. Dim uses an exclusive upper-bound (so we add 1)
    let fresh' = unionBoxes [Dim lo (hi + 1) Pt | (lo, hi) <- fresh]
        isFresh x = any (inRange x) fresh'

    print (countBy isFresh input)
    print (sum (map size fresh'))

inRange :: Int -> Box' 1 -> Bool
inRange x (Dim lo hi Pt) = lo <= x && x < hi
