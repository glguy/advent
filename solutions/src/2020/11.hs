{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/11>

-}
module Main (main) where

import Advent
import Advent.Coord
import Data.Array.Base qualified as AB
import Data.Array.Unboxed qualified as A
import Data.Maybe (mapMaybe)

type Seating   = A.UArray Coord Char

-- | Neighbors are stored using underlying raw indexes for fast access
-- on the 'Seating' grid.
type Neighbors = A.Array Coord [Int]

-- |
-- >>> :main
-- 2211
-- 1995
main :: IO ()
main =
  do inp <- getInputArray 11
     let run f = print (count '#' (A.elems (stable f inp)))
     run (adv 4 (adjacent inp))
     run (adv 5 (lineOfSight inp))

-- | Repeatedly apply the function until it returns 'Nothing'. Return the
-- argument that returned 'Nothing'.
stable :: (a -> Maybe a) -> a -> a
stable f x = maybe x (stable f) (f x)

-- | Immediate neighbors used in part 1
adjacent :: Seating -> Neighbors
adjacent a = A.listArray b [[A.index b j | j <- neighbors i, A.inRange b j] | i <- A.range b]
  where
    b = A.bounds a

-- | Line of sight neighbors used in part 2
lineOfSight :: Seating -> Neighbors
lineOfSight a = A.listArray b [mapMaybe (look i) (neighbors origin) | i <- A.range b]
  where
    b = A.bounds a
    look i d =
      do let j = i + d
         v <- arrIx a j
         case v of
           '.' -> look j d
           _   -> Just (A.index b j)

-- | Advance the seating grid one time step using a configurable
-- threshold for seats becoming unoccupied, a precomputed neighborhood,
-- and the current seating chart. Return 'Nothing' when nothing changes.
adv ::
  Int           {- ^ occupied neighbor threshold      -} ->
  Neighbors     {- ^ neighborhood for each coordinate -} ->
  Seating       {- ^ current seating grid             -} ->
  Maybe Seating {- ^ updated seating grid             -}
adv t ns a
  | null changes = Nothing
  | otherwise    = Just $! a A.// changes
  where
    changes = [(i, v) | (i, e) <- A.assocs a, v <- valueAt i e]

    -- returns True when /at least/ n neighbors are occupied
    occupied :: Int -> Coord -> Bool
    occupied n i = occupied1 n (ns A.! i)

    occupied1 0 _  = True
    occupied1 _ [] = False
    occupied1 n (i:is) =
      case AB.unsafeAt a i of
        '#' -> occupied1 (n-1) is
        _   -> occupied1 n is

    valueAt i '#' | occupied t i       = "L"
    valueAt i 'L' | not (occupied 1 i) = "#"
    valueAt _ _                        = ""
