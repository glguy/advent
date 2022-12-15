{-# Language QuasiQuotes, DataKinds, NumericUnderscores, GADTs #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/15>

-}
module Main (main) where

import Advent (format)
import Advent.Box (Box(Dim,Pt), subtractBox, size)
import Advent.Coord (manhattan, Coord(C))
import Advent.Nat (Nat(Z, S))

-- | Input is a list of: sensor x and y, beacon x and y
type Input = [(Int,Int,Int,Int)]

-- |
-- >>> :main
-- 4724228
-- 13622251246513
main :: IO ()
main =
 do input <- [format|2022 15 (Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d%n)*|]
    print (part1 input)
    print (part2 input)

-- part 1 logic

part1 :: Input -> Int
part1 input =
  let p1y = 2_000_000 in
  sum $ map size $
  subtractAllOf (beaconsAtY input p1y) $
  makeDisjoint $
  concatMap (ranges p1y) input

beaconsAtY :: Input -> Int -> [Box ('S 'Z)]
beaconsAtY input y = [cover bx 0 Pt | (_,_,bx,by) <- input, by == y]

ranges :: Int -> (Int,Int,Int,Int) -> [Box ('S 'Z)]
ranges y (sx,sy,bx,by) = [cover sx dx Pt | dx >= 0]
  where
    dy = abs (y - sy)
    dx = r - dy
    r  = manhattan (C sy sx) (C by bx)

-- part 2 logic

part2 :: Input -> Int
part2 input = head
  [ 4_000_000 * x + y
    | C y x <-
        map fromDiamond $
        subtractAllOf (toDiamonds input)
        [toDiamond (C 2_000_000 2_000_000) 4_000_000]
    , 0 <= y, y <= 4_000_000, 0 <= x, x <= 4_000_000]

-- | Find a corner of a diamond represented as a square region.
fromDiamond :: Box ('S ('S 'Z)) -> Coord
fromDiamond (Dim xpy _ (Dim xmy _ Pt)) = C ((xpy - xmy) `div` 2) ((xpy + xmy) `div` 2) 

-- | Covert a diamond centered at a coordinate with a radius into a square region.
toDiamond :: Coord -> Int -> Box ('S ('S 'Z))
toDiamond (C y x) r = cover (x+y) r (cover (x-y) r Pt)

-- | Convert all the sensors in the input file into square regions corresponding to the
-- diamond covered by the sensor.
toDiamonds :: Input -> [Box ('S ('S 'Z))]
toDiamonds input =
  [toDiamond (C sy sx) (manhattan (C sy sx) (C by bx)) | (sx,sy,bx,by) <- input]

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

-- | Given a list of potentially overlapping boxes create a new list
-- of boxes that cover the same region but which do not overlap
makeDisjoint :: [Box a] -> [Box a]
makeDisjoint = foldr add []
  where
    add box rest = box : concatMap (subtractBox box) rest
