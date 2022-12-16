{-# Language GADTs, DataKinds, LambdaCase, BlockArguments, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/19>

To correlate all the scanner readings this program
selects the first scanner to be "correct". All other
scanners will be oriented relative to the first scanner.
As each scanner's location is fixed it will be queued
to be compared to all the uncorrelated scanner outputs.
Scanning in this order ensures no pair of scanners is
compared more than once.

-}
module Main (main) where

import Advent (format, counts)
import Advent.Box
import Advent.Coord3 (Coord3(..), origin, diff, add)
import Advent.Nat
import Control.Monad ((>=>))
import Data.Either (partitionEithers)
import Data.List (transpose)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 457
-- 13243
main :: IO ()
main =
 do inp <- [format|2021 19 (--- scanner %u ---%n(%d,%d,%d%n)*)&%n|]
    let coord (x,y,z) = C3 x y z
    let scanners = [map coord ps | (_,ps) <- inp]

    let (offsets, locations) = unzip (start scanners)
    print (Set.size (Set.unions locations))
    print (radius offsets)

-- | Starts the scanner reading correlation algorithm.
start ::
  [[Coord3]] {- ^ uncorrelated scanner readings -} ->
  [(Coord3, Set Coord3)] {- ^ correlated scanner locations and readings -}
start (x:xs) = assemble xs [(origin, Set.fromList x)]
start [] = []

-- | Worker for 'start'.
assemble ::
  [[Coord3]]             {- ^ uncorrelated scanner readings -} ->
  [(Coord3, Set Coord3)] {- ^ recently correlated scanners -} ->
  [(Coord3, Set Coord3)] {- ^ completed correlated locations and readings -}
assemble _ [] = []
assemble remains (c@(_,reference):cs) = c : assemble remain' (new ++ cs)
  where
    (new,remain') = partitionEithers
      [ maybe (Right remain) Left (match reference remain)
        | remain <- remains
      ]

-- | Try to match the uncorrelated offsets to a set of absolute coordinates.
match ::
  Set Coord3 {- ^ reference coordinates -} ->
  [Coord3]   {- ^ uncorrelated offsets -} ->
  Maybe (Coord3, Set Coord3) {- ^ sensor offset and absolute beacons -}
match xset ys = listToMaybe
 [(offset, yset')
   | yset <- Set.fromList <$> reorient ys
   , offset <- prefilter (diff <$> Set.toList xset <*> Set.toList yset)
   , let yset' = Set.mapMonotonic (add offset) yset
   , 12 <= Set.size (Set.intersection xset yset')
 ]

-- | Only bother checking offsets that occur enough times that it's possible
-- to have an overlap.
prefilter :: [Coord3] -> [Coord3]
prefilter = Map.keys . Map.filter (>= 12) . counts

-- * Reorienting sensor readings

-- | Return all 24 possibilities of rotating the given list of coordinates.
reorient :: [Coord3] -> [[Coord3]]
reorient = transpose . map (rotations >=> faces)

faces :: Coord3 -> [Coord3]
faces (C3 x y z) =
  [
    C3 x y z,
    C3 y (-x) z,
    C3 (-x) (-y) z,
    C3 (-y) x z,
    C3 y z x,
    C3 y (-z) (-x)
  ]

-- | Return the 4 rotations of a point around the x-axis
rotations :: Coord3 -> [Coord3]
rotations (C3 x y z) =
  [
    C3 x y z,
    C3 x (-z) y,
    C3 x (-y) (-z),
    C3 x z (-y)
  ]

-- * Determining sensor radius

-- | Determines the maximum manhattan distance between any pair of points.
-- this is achieved by finding the bounding octahedroid for this set of points.
radius :: [Coord3] -> Int
radius = minCube . coverBoxes . map to4

-- | Find the side length of the smallest hypercube that can bound
-- the given hyperrectangle.
minCube :: Box n -> Int
minCube (Dim a b x) = max (b-a) (minCube x)
minCube Pt = 0

-- | Convert a 3D point into an octahedron coordinate.
to4 :: Coord3 -> Box ('S ('S ('S ('S 'Z))))
to4 (C3 x y z) = x+y+z # x+y-z # x-y+z # x-y-z # Pt
  where
    i # j = Dim i i j
    infixr 5 #
