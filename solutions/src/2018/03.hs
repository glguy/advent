{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/3>

Given a bunch of 2D regions find regions that do and do not overlap.

-}
module Main (main) where

import Advent (format, pickOne)
import Advent.Box (Box(Dim, Pt), size, unionBoxes, intersectBox)
import Data.List (tails)
import Data.Maybe (isNothing)

-- | Print the answers to part 1 and 2 of day 3's task.
--
-- >>> :main
-- 115304
-- 275
main :: IO ()
main =
 do input <- [format|2018 3 (#%u %@ %u,%u: %ux%u%n)*|]
    let boxes = [(i, Dim x (x+sx) (Dim y (y+sy) Pt)) | (i,x,y,sx,sy) <- input]
    print (part1 (map snd boxes))
    print (part2 boxes)

-- | Determine the size of the region covered by more than one patch
part1 :: [Box n] -> Int
part1 boxes =
  sum (map size (unionBoxes
  [q | p:ps <- tails boxes, Just q <- map (intersectBox p) ps]))

-- | Determine identifier of patch with no overlaps.
part2 :: [(i, Box n)] -> i
part2 boxes =
  head [i | ((i,p),ps) <- pickOne boxes, all (isNothing . intersectBox p . snd) ps]
