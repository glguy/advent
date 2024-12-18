{-# Language QuasiQuotes, ImportQualifiedPost, BlockArguments #-}
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

import Advent (arrIx, format, binSearchSmallest)
import Advent.Coord (Coord(..), cardinal, manhattan)
import Advent.Search (AStep(AStep), astar)
import Data.Array.Unboxed (UArray, accumArray)
import Data.Maybe (isNothing, listToMaybe)

-- | >>> :main
-- 278
-- 43,12
main :: IO ()
main =
 do input <- [format|2024 18 (%u,%u%n)*|]
    let Just cost = search (take 1024 input)
    print cost
    let isBlocking i = isNothing (search (take i input))
        needed       = binSearchSmallest isBlocking 1024 (length input)
        (x,y)        = input !! (needed - 1)
    putStrLn (show x ++ "," ++ show y)

-- | Find the minimum cost to go from one side of the maze to the other, if there is one.
search :: [(Int, Int)] -> Maybe Int
search points = listToMaybe [cost | (C 70 70, cost) <- astar step (C 0 0)]
  where
    open :: UArray Coord Bool
    open = accumArray (\_ e -> e) True (C 0 0, C 70 70) [(C y x, False) | (x,y) <- points]

    step i = [AStep j 1 (manhattan j (C 70 70)) | j <- cardinal i, True <- arrIx open j]
