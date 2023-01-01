{-# Language QuasiQuotes, DataKinds, NumericUnderscores, GADTs #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/15>

>>> :{
:main +
  "part 1 override 10 part 2 override 20\n\
  \Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n\
  \Sensor at x=9, y=16: closest beacon is at x=10, y=16\n\
  \Sensor at x=13, y=2: closest beacon is at x=15, y=3\n\
  \Sensor at x=12, y=14: closest beacon is at x=10, y=16\n\
  \Sensor at x=10, y=20: closest beacon is at x=10, y=16\n\
  \Sensor at x=14, y=17: closest beacon is at x=10, y=16\n\
  \Sensor at x=8, y=7: closest beacon is at x=2, y=10\n\
  \Sensor at x=2, y=0: closest beacon is at x=2, y=10\n\
  \Sensor at x=0, y=11: closest beacon is at x=2, y=10\n\
  \Sensor at x=20, y=14: closest beacon is at x=25, y=17\n\
  \Sensor at x=17, y=20: closest beacon is at x=21, y=22\n\
  \Sensor at x=16, y=7: closest beacon is at x=15, y=3\n\
  \Sensor at x=14, y=3: closest beacon is at x=15, y=3\n\
  \Sensor at x=20, y=1: closest beacon is at x=15, y=3\n"
:}
26
56000011

-}
module Main where

import Advent (format)
import Advent.Box (Box', Box(Dim,Pt), subtractBox, size, unionBoxes)
import Advent.Coord (manhattan, Coord(C))
import Advent.Nat (Nat(S))

-- | Input is a list of: sensor x and y, beacon x and y
type Input = [(Int,Int,Int,Int)]

-- |
-- >>> :main
-- 4724228
-- 13622251246513
main :: IO ()
main =
 do (cfg,input) <- [format|2022 15
      (|part 1 override %u part 2 override %u%n)
      (Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d%n)*|]
    let (diamonds, beacons) = unzip (inputSensors input)
    print (part1 (maybe 2_000_000 fst cfg) diamonds beacons)
    print (part2 (maybe 4_000_000 snd cfg) diamonds)

-- | A sensor has a location and a radius
data Sensor = Sensor Coord Int

-- | Convert input data into a list of sensors and beacon coordinates
inputSensors :: Input -> [(Sensor, Coord)]
inputSensors input =
  [(Sensor s (manhattan s b), b) | (sx,sy,bx,by) <- input, let s = C sy sx, let b = C by bx]

-- part 1 logic

-- | Compute the number of locations in a given row that can't contain a sensor.
part1 ::
  Int      {- ^ y value of row -} ->
  [Sensor] {- ^ sensors -} ->
  [Coord]  {- ^ beacons -} ->
  Int      {- ^ locations in row that can't contain a sensor -}
part1 row diamonds beacons =
  sum $ map size $
  subtractAllOf [cover x 0 Pt | C y x <- beacons, y == row] $
  unionBoxes $
  concatMap (rowSlice row) diamonds

-- | Generate the 1-d box describing the X region covered by the sensor at a given Y value
rowSlice ::
  Int           {- ^ y value            -} ->
  Sensor        {- ^ sensor             -} ->
  [Box' 1] {- ^ bounds on x values -}
rowSlice y (Sensor (C sy sx) r) = [cover sx dx Pt | dx >= 0]
  where
    dy = abs (y - sy)
    dx = r - dy

-- part 2 logic

-- | Find the tuning frequency of the only location in the given region that could
-- contain an undiscovered beacon.
part2 ::
  Int      {- ^ search region size -} ->
  [Sensor] {- ^ sensors -} ->
  Int      {- ^ tuning frequency -}
part2 search diamonds = head
  [ 4_000_000 * x + y
    | let center = C (search`div`2) (search`div`2)
    , C y x <-
        map boxCorner $
        subtractAllOf (map diamondBox diamonds)
        [diamondBox (Sensor center search)]
    , 0 <= y, y <= search
    , 0 <= x, x <= search]

-- | Find a corner of a diamond represented as a square region.
boxCorner :: Box' 2 -> Coord
boxCorner (Dim xpy _ (Dim xmy _ _)) = C ((xpy - xmy) `div` 2) ((xpy + xmy) `div` 2)

-- | Covert a diamond centered at a coordinate with a radius into a square region.
diamondBox :: Sensor -> Box' 2
diamondBox (Sensor (C y x) r) = cover (x+y) r (cover (x-y) r Pt)

-- Box utilities

-- | Remove the first list of regions from the second.
subtractAllOf ::
  [Box n] {- ^ remove this -} ->
  [Box n] {- ^ from this -} ->
  [Box n] {- ^ remaining region -}
subtractAllOf xs ys = foldl remove1 ys xs
  where
    remove1 acc x = concatMap (subtractBox x) acc

-- | Extend a box to cover a new dimension centered on x with radius r.
cover :: Int {- ^ position -} -> Int {- ^ radius -} -> Box n -> Box ('S n)
cover x r = Dim (x - r) (x + r + 1)
