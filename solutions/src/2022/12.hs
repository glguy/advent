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

-- |
-- >>> :main
-- 528
-- 522
main :: IO ()
main =
 do input <- getInputArray 2022 12
    print (solve input 'S')
    print (solve input 'a')

solve :: UArray Coord Char -> Char -> Int
solve input startLetter =
    head [n | (e,n) <- bfsOnN fst (step input) startStates, input!e == 'E']
    where
        startStates = [(k,0) | (k,v) <- assocs input, v==startLetter]

step :: UArray Coord Char -> (Coord,Int) -> [(Coord,Int)]
step a (here, n) =
    [ (next,n+1)
    | next      <- cardinal here
    , Just dest <- [arrIx a next]
    , succ (elevation (a!here)) >= elevation dest
    ]

-- | Compute the logical elevation by mapping start and end characters to
-- their corresponding lowercase elevation values.
elevation :: Char -> Char
elevation 'E' = 'z'
elevation 'S' = 'a'
elevation x   = x