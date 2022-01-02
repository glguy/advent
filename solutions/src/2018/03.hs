{-# Language QuasiQuotes, ImportQualifiedPost, ViewPatterns #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/3>

-}
module Main (main) where

import Advent (format, pickOne)
import Advent.Box (Box(..), size, intersectBox, subtractBox)
import Data.Foldable (Foldable(toList))
import Data.List (tails)
import Data.Maybe (isNothing)

-- | Print the answers to part 1 and 2 of day 3's task.
--
-- >>> :main
-- 115304
-- 275
main :: IO ()
main =
 do input <- [format|3 (#%u %@ %u,%u: %ux%u%n)*|]
    let boxes = [(i, Dim x (x+sx) (Dim y (y+sy) Pt)) | (i,x,y,sx,sy) <- input]
    print (regionSize (overlaps (map snd boxes)))
    print (noOverlaps boxes)

-- | Determine all pair-wise overlapping regions between the list of patches.
overlaps :: [Box n] -> [Box n]
overlaps boxes =
  [q | p:ps <- tails boxes, (intersectBox p -> Just q) <- ps]

-- | Determine identifier of patch with no overlaps.
noOverlaps :: [(i, Box n)] -> i
noOverlaps boxes =
  head [i | ((i,p),ps) <- pickOne boxes, all (isNothing . intersectBox p . snd) ps]

-- | Determine the size of a region covered by boxes.
regionSize :: [Box n] -> Int
regionSize = sum . map size . foldl addBox []
  where
    addBox xs box = box : (subtractBox box =<< xs)
