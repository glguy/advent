{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/12>

>>> :{
:main +
    "Sabqponm\n\
    \abcryxxl\n\
    \accszExk\n\
    \acctuvwj\n\
    \abdefghi\n"
:}
31
29

-}
module Main where

import Advent (getInputArray, arrIx)
import Advent.Coord (Coord, cardinal)
import Advent.Search (bfsOnN)
import Data.Array.Unboxed (UArray, (!), assocs, amap)

-- |
-- >>> :main
-- 528
-- 522
main :: IO ()
main =
 do input <- getInputArray 2022 12
    print (solve input 'S')
    print (solve input 'a')

-- | Given an input map and a starting letter, return the length of the shortest
-- path to the ending letter (@E@).
solve :: UArray Coord Char -> Char -> Int
solve input startLetter =
    head [n | (e, n) <- bfsOnN fst step startStates, input ! e == 'E']
    where
        startStates = [(k, 0) | (k, v) <- assocs input, v == startLetter]
        elevations  = amap elevation input
        
        step (here, n) =
            [ (next, n+1)
            | next <- cardinal here
            , dest <- arrIx elevations next
            , succ (elevations ! here) >= dest
            ]

-- | Compute the logical elevation by mapping start and end characters to
-- their corresponding lowercase elevation values.
elevation :: Char -> Char
elevation 'S' = 'a'
elevation 'E' = 'z'
elevation  x  =  x
