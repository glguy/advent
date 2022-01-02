{-# Language QuasiQuotes, ImportQualifiedPost, ViewPatterns #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/3>

-}
{-# Language OverloadedStrings #-} -- for parser
module Main (main) where

import Advent (format, pickOne)
import Advent.Box (Box(..), size, intersectBox, subtractBox)
import Data.List (tails)
import Data.Foldable (Foldable(toList))
import Data.Maybe (isNothing)

-- | Print the answers to part 1 and 2 of day 3's task.
--
-- >>> :main
-- 115304
-- 275
main :: IO ()
main =
 do input <- [format|3 (#%u %@ %u,%u: %ux%u%n)*|]
    let boxes = [(i,Dim x (x+sx) (Dim y (y+sy) Pt)) | (i,x,y,sx,sy) <- input]
    print (overlaps (map snd boxes))
    print (noOverlaps boxes)

-- | Determine region size of area covered by more than one patch.
overlaps :: [Box n] -> Int
overlaps boxes = regionSize [o | p:ps <- tails boxes, q <- ps, o <- toList (intersectBox p q)]

-- | Determine identifier of patch with no overlaps.
noOverlaps :: [(a, Box n)] -> a
noOverlaps boxes =
  head [i | ((i,p),ps) <- pickOne boxes, all (isNothing . intersectBox p . snd) ps]

-- | Determine the size of a region covered by boxes.
regionSize :: Foldable t => t (Box n) -> Int
regionSize = sum . map size . foldl f []
  where
    f xs box = box : concatMap (subtractBox box) xs
