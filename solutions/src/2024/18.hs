{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/18>

-}
module Main where

import Advent (arrIx, format)
import Advent.Coord (Coord(..), cardinal, manhattan, right, below)
import Advent.DisjointSet (newDisjointSet, unifySets, inSameSet)
import Advent.Search (AStep(AStep), astar)
import Control.Monad (when)
import Data.Array.IO (IOUArray, Ix(range, inRange), readArray, writeArray, newArray)
import Data.Array.Unboxed (UArray, accumArray)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.Maybe (listToMaybe)

-- | >>> :main
-- 278
-- 43,12
main :: IO ()
main =
 do input <- [format|2024 18 (%u,%u%n)*|]
    let cs = [C y x | (x,y) <- input]
    let Just cost = search (take 1024 cs)
    print cost
    print =<< part2 cs

-- | Find the minimum cost to go from one side of the maze to the other, if there is one.
search :: [Coord] -> Maybe Int
search points = listToMaybe [cost | (C 70 70, cost) <- astar step (C 0 0)]
  where
    open :: UArray Coord Bool
    open = accumArray (\_ e -> e) True (C 0 0, C 70 70) [(c, False) | c <- points]

    step i = [AStep j 1 (manhattan j (C 70 70)) | j <- cardinal i, True <- arrIx open j]

-- | Find the coordinate that when added creates a path from start to end introducing
-- them one at a time in reverse order.
part2 :: [Coord] -> IO Coord
part2 cs =
 do let b = (C 0 0, C 70 70)
    ds <- newDisjointSet b
    open <- newArray b True :: IO (IOUArray Coord Bool)

    -- Mark all of the points that will be added as initially unavailable
    for_ cs \c -> writeArray open c False

    -- Try to connect two locations if they are both open and on the grid
    let link x y =
         when (inRange b y)
          do o <- readArray open y
             when o (void (unifySets ds x y))

    -- Connect all the adjacent, initially open spaces
    for_ (range b) \c ->
     do o <- readArray open c
        when o
         do link c (right c)
            link c (below c)

    -- remove obstructions one by one testing for connectivity along the way
    let go [] = fail "no solution"
        go (x:xs) =
         do writeArray open x True
            traverse_ (link x) (cardinal x)
            done <- inSameSet ds (C 0 0) (C 70 70)
            if done then pure x else go xs

    go (reverse cs)
