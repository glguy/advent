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

import Advent (format)
import Advent.Search (bfs)
import Advent.Coord3 (Coord3(..), boundingBox)

-- |
-- >>> :main
-- 4332
-- 2524
main :: IO ()
main =
 do input <- [format|2022 18 (%u,%u,%u%n)*|]
    let cubes    = Set.fromList (map toC3 input)
    let air      = findAir cubes
    print (length [() | c <- Set.toList cubes, n <- neigh c, Set.notMember n cubes])
    print (length [() | c <- Set.toList cubes, n <- neigh c, Set.member    n air  ])

-- | Given the the location of the lava cubes, find a bounding box of air surrounding them.
findAir :: Set Coord3 -> Set Coord3
findAir cubes = Set.fromList (bfs step (hi + 1))
  where
    (lo, hi) = fromJust (boundingBox cubes)
    box      = (lo - 1, hi + 1)
    step c   = [n | n <- neigh c, inRange box n, Set.notMember n cubes]

-- | Neighbors of the cubes (excluding diagonals)
neigh :: Coord3 -> [Coord3]
neigh (C3 x y z) = [C3 (x+1) y z, C3 (x-1) y z, C3 x (y+1) z, C3 x (y-1) z, C3 x y (z+1), C3 x y (z-1)]

-- | Convert tuple to Coord3
toC3 :: (Int, Int, Int) -> Coord3
toC3 (x,y,z) = C3 x y z
