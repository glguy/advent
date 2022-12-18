{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/18>

>>> :{
:main +
    "2,2,2\n\
    \1,2,2\n\
    \3,2,2\n\
    \2,1,2\n\
    \2,3,2\n\
    \2,2,1\n\
    \2,2,3\n\
    \2,2,4\n\
    \2,2,6\n\
    \1,2,5\n\
    \3,2,5\n\
    \2,1,5\n\
    \2,3,5\n"
:}
64
58

-}
module Main where

import Data.Ix (inRange)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

import Advent
import Advent.Search ( bfs )
import Advent.Coord3

-- |
-- >>> :main
-- 4332
-- 2524
main :: IO ()
main =
 do input <- map toC3 <$> [format|2022 18 (%u,%u,%u%n)*|]
    let cubes = Set.fromList input
    let (lo,hi) = fromJust (boundingBox (Set.toList cubes))
    let bnds = (lo - 1, hi + 1)
    let air = Set.fromList (bfs (step bnds cubes) (hi + 1))
    print $ length [n | c <- input, n <- neigh c, Set.notMember n cubes]
    print $ length [n | c <- input, n <- neigh c, Set.member n air]

toC3 :: (Int, Int, Int) -> Coord3
toC3 (x,y,z) = C3 x y z

-- | Return all the adjacent air boxes.
step :: (Coord3, Coord3) -> Set Coord3 -> Coord3 -> [Coord3]
step box cubes c = [n | n <- neigh c, inRange box n, Set.notMember n cubes]

-- | Neighbors of the cubes (excluding diagonals)
neigh :: Coord3 -> [Coord3]
neigh (C3 x y z) = [C3 (x+1) y z, C3 (x-1) y z, C3 x (y+1) z, C3 x (y-1) z, C3 x y (z+1), C3 x y (z-1)]
