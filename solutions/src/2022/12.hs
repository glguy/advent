{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/12>

-}
module Main where

import Advent (getInputArray, arrIx)
import Advent.Coord (Coord, cardinal)
import Advent.Search (bfsOnN)
import Data.Array.Unboxed (UArray, (!), assocs)

main :: IO ()
main =
 do input <- getInputArray 2022 12
    print (solve input 'S')
    print (solve input 'a')

solve :: UArray Coord Char -> Char -> Int
solve input startLetter =
    minimum [n | (e,n) <- bfsOnN fst (step input) startStates, input!e == 'E']
    where
        startStates = [(k,0) | (k,v) <- assocs input, v==startLetter]

step :: UArray Coord Char -> (Coord,Int) -> [(Coord,Int)]
step a (here, n) =
    [ (next,n+1)
    | next      <- cardinal here
    , Just dest <- [arrIx a next]
    , a!here == 'S' || dest == 'E' || succ (a!here) >= dest
    ]
    