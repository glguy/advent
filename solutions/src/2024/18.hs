{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/18>

Uses a binary search to reduce the number of reachable searchs
that need to be done.

-}
module Main where

import Advent (arrIx, format, binSearchLargest)
import Advent.Coord (Coord(..), cardinal)
import Advent.Search (AStep(AStep), astar)
import Data.Array.Unboxed (UArray, accumArray)
import Data.List (find, inits)
import Data.Maybe (isNothing, listToMaybe)

-- | >>> :main
-- 278
-- 43,12
main :: IO ()
main =
 do input <- [format|2024 18 (%u,%u%n)*|]
    let Just cost = search (take 1024 input)
    print cost
    let input'       = reverse input
        isBlocking i = isNothing (search (drop i input'))
        (x,y)        = input' !! binSearchLargest isBlocking 0 (length input' - 1024)
    putStrLn (show x ++ "," ++ show y)

-- | Find the minimum cost to go from one side of the maze to the other, if there is one.
search :: [(Int, Int)] -> Maybe Int
search points = listToMaybe [cost | (C 70 70, cost) <- astar step (C 0 0)]
  where
    open :: UArray Coord Bool
    open = accumArray (\_ e -> e) True (C 0 0, C 70 70) [(C y x, False) | (x,y) <- points]

    step i = [AStep j 1 0 | j <- cardinal i, True <- arrIx open j]
