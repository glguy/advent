{-# Language QuasiQuotes, ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/11>

This solution uses dynamic programming to efficiently compute
the area of an arbitrary square on the power-cell grid.

This efficiency is achieved using a summed-area table.
<https://en.wikipedia.org/wiki/Summed-area_table>

Once we've computed this table we're able compute the sum of
the elements contained in an arbitrary rectangle via addition
and subtraction of 4 elements.

-}
module Main (main) where

import Advent.Coord (Coord(C))
import Advent.Format (format)
import Data.Array.Unboxed qualified as A
import Data.Foldable (foldl')
import Data.List (intercalate)

type Grid = A.UArray Coord Int

dim :: Int
dim = 300

-- | Print the answers to day 11
--
-- >>> :main
-- 241,40
-- 166,75,12
main :: IO ()
main =
  do serial <- [format|11 %u%n|]
     let table = summedAreaTable (powerLevel serial)
     putStrLn (part1 table)
     putStrLn (part2 table)

-- | Compute the position of the largest-valued 3-square
--
-- >>> part1 (summedAreaTable (powerLevel 18))
-- "33,45"
-- >>> part1 (summedAreaTable (powerLevel 42))
-- "21,61"
part1 :: Grid -> String
part1 areas =
  case maximumSquare areas 3 3 of
    (C y x, _) -> intercalate "," (map show [x,y])    

-- | Compute the position and size of the largest-valued square on the grid
--
-- >>> part2 (summedAreaTable (powerLevel 18))
-- "90,269,16"
-- >>> part2 (summedAreaTable (powerLevel 42))
-- "232,251,12"
part2 :: Grid -> String
part2 areas =
  case maximumSquare areas 1 dim of
    (C y x, s) -> intercalate "," (map show [x,y,s])

-- | Compute power level of power cell given the cells coordinate
-- and the player's serial number.
--
-- >>> powerLevel 8 (C 5 3)
-- 4
-- >>> powerLevel 57 (C 79 122)
-- -5
-- >>> powerLevel 39 (C 196 217)
-- 0
-- >>> powerLevel 71 (C 153 101)
-- 4
powerLevel ::
  Int   {- ^ serial number   -} ->
  Coord {- ^ grid coordinate -} ->
  Int   {- ^ power level     -}
powerLevel serial (C y x) = (rackid * y + serial) * rackid `div` 100 `mod` 10 - 5
  where
    rackid = x+10

-- | Compute the coordinates and size of the square on the
-- given Grid that maximizes the sum of the contained power levels.
maximumSquare ::
  Grid         {- ^ summed area grid              -} ->
  Int          {- ^ candidate size lower bound    -} ->
  Int          {- ^ candidate size upper bound    -} ->
  (Coord, Int) {- ^ coordinate and size of square -}
maximumSquare areas sizelo sizehi =
  fst $ maximize
    [((C y x, size), area)
      | size <- [sizelo .. sizehi]
      , x    <- [1 .. dim-size-1]
      , y    <- [1 .. dim-size-1]
      , let !area = rectangleSum areas (C y x) size size
      ]

  -- I'm using the loops package here because it saves time
  -- over generating a large list. In addition, maximumBy
  -- is less performant than writing maximize out with a strict
  -- fold.
  where
    maximize = foldl' (\x y -> if snd x > snd y then x else y) ((C 0 0, 0), minBound)

-- | Compute the sum of the power levels contained in a rectangle
-- specified by its top-left corner coordinate, width, and height.
rectangleSum ::
  Grid  {- ^ summed area grid              -} ->
  Coord {- ^ top-left corner of rectangle  -} ->
  Int   {- ^ rectangle width               -} ->
  Int   {- ^ rectangle height              -} ->
  Int   {- ^ sum of power levels in region -}
rectangleSum areas (C y x) w h
  = areas A.! C (y+h-1) (x+w-1)
  - areas A.! C (y+h-1) (x  -1)
  - areas A.! C (y  -1) (x+w-1)
  + areas A.! C (y  -1) (x  -1)

-- | Compute the summed-area table given a function that maps
-- a coordinate to its value. This table can be used with 'rectangleSum'
-- to efficiently compute the area of a rectangular region of the grid.
--
-- Each position in this grid is the sum of the values of all power
-- levels above and to the left of the current position (inclusive).
--
-- See the module header for more information about summed-area tables
-- and what we're doing with one.
summedAreaTable ::
  (Coord -> Int) {- ^ value for coordinate -} ->
  Grid           {- ^ summed-area table    -}
summedAreaTable f = convert table -- convert to unboxed array
  where
  -- table is boxed to allow self-reference in definition
  table :: A.Array Coord Int
  table = A.array (C 0 0, C dim dim)
        $ concat [ [ (C 0 z, 0), (C z 0, 0)] | z <- [0..dim]]
        ++
        [ ( C y x
          , f (C y x)
          + table A.! C  y    (x-1)
          + table A.! C (y-1)  x
          - table A.! C (y-1) (x-1)
          )
        | x <- [1..dim]
        , y <- [1..dim]
        ]

-- | Convert between different array representations.
convert :: (A.IArray a e, A.IArray a' e, A.Ix i) => a i e -> a' i e
convert a = A.array (A.bounds a) (A.assocs a)
