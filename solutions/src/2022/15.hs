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

import Advent ( format )
import Advent.Box
import Advent.Coord ( manhattan, Coord(C) )
import Advent.Nat ( Nat(Z, S) )

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
  removeallof (beaconsAtY input p1y) $
  makeDisjoint [y | x <- input, y <- ranges p1y x]

beaconsAtY :: Input -> Int -> [Box ('S 'Z)]
beaconsAtY input ty = [Dim nx (nx+1) Pt | (_,_,nx,ny)<-input, ny == ty]

ranges :: Int -> (Int,Int,Int,Int) -> [Box ('S 'Z)]
ranges yy (x,y,nx,ny)
  | dx < 0 = []
  | otherwise = [cover x dx Pt]
    where
        dy = abs (yy - y)
        dx = dist - dy
        dist = manhattan (C y x) (C ny nx)

-- part 2 logic

part2 :: Input -> Int
part2 input = head
  [ 4_000_000 * y + x
    | C y x <-
        map fromdiamond $
        removeallof (todiamonds input)
        [todiamond (C 2_000_000 2_000_000) 4_000_000]
    , 0 <= y, y <= 4_000_000, 0 <= x, x <= 4_000_000]

-- | Find a corner of a diamond represented as a square region.
fromdiamond :: Box ('S ('S 'Z)) -> Coord
fromdiamond (Dim xpy _ (Dim xmy _ Pt)) = C ((xpy + xmy) `div` 2) ((xpy - xmy) `div` 2)

-- | Covert a diamond centered at a coordinate with a radius into a square region.
todiamond :: Coord -> Int -> Box ('S ('S 'Z))
todiamond (C y x) r = cover (x+y) r (cover (x-y) r Pt)

-- | Convert all the sensors in the input file into square regions corresponding to the
-- diamond covered by the sensor.
todiamonds :: Input -> [Box ('S ('S 'Z))]
todiamonds input =
  [ todiamond (C y x) r
     | (x,y,nx,ny) <- input
     , let r = manhattan (C y x) (C ny nx)
     ]

-- Box utilities

-- | Remove the first list of regions from the second.
removeallof ::
  [Box n] {- ^ remove this -} ->
  [Box n] {- ^ from this -} ->
  [Box n] {- ^ remaining region -}
removeallof xs ys = foldl remove1 ys xs
  where remove1 acc x = concatMap (subtractBox x) acc

-- | Extend a box to cover a new dimension centered on x with radius r.
cover :: Int {- ^ position -} -> Int {- ^ radius -} -> Box n -> Box ('S n)
cover x r = Dim (x - r) (x + r + 1)

-- | Given a list of potentially overlapping boxes create a new list
-- of boxes that cover the same region but which do not overlap
makeDisjoint :: [Box a] -> [Box a]
makeDisjoint = foldr add []
  where
    add x sofar = x : concatMap (subtractBox x) sofar
