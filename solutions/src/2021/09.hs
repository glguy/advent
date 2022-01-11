{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/9>

Find the basins on the height map.

>>> :{
exampleGrid =
  Data.Array.Unboxed.listArray
    (C 0 0, C 4 9)
    [2,1,9,9,9,4,3,2,1,0,
     3,9,8,7,8,9,4,9,2,1,
     9,8,5,6,7,8,9,8,9,2,
     8,7,6,7,8,9,6,7,8,9,
     9,8,9,9,9,6,5,6,7,8]
:}

>>> exampleBasins = toBasinIds exampleGrid

>>> basinRiskSum exampleGrid exampleBasins
15

>>> basinSizes exampleBasins
[3,9,14,9]

-}
module Main (main) where

import Advent (counts, getInputArray, arrIx)
import Advent.Coord (Coord(..), cardinal)
import Data.Array.Unboxed (Array, UArray, (!), amap, array, assocs, bounds, elems)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (sortBy)
import Data.Maybe (catMaybes)

-- | >>> :main
-- 588
-- 964712
main :: IO ()
main =
 do heights <- amap digitToInt <$> getInputArray 2021 9
    let basinIds = toBasinIds heights
    print (basinRiskSum heights basinIds)
    print (product (top 3 (basinSizes basinIds)))

-- * Basins

-- | Array of locations in the grid and the unique low point they flow towards.
type BasinIds = Array Coord (Maybe Coord)

-- | Compute the basin low points for each location on the grid.
toBasinIds :: UArray Coord Int -> BasinIds
toBasinIds heights = basinIds
  where
    basinIds = array (bounds heights) [(c, basinId c h) | (c, h) <- assocs heights]
    
    basinId _ 9 = Nothing -- problem defines height 9 not to be in a basin
    basinId c h =
     do xs <- sequence [basinIds!x | x <- cardinal c, xh <- arrIx heights x, xh<h]
        case xs of
          []                  -> Just c -- this is the lowest point in the basin
          y:ys | all (y==) ys -> Just y -- all flows go to a unique basin
               | otherwise    -> Nothing -- no unique basin

-- | Compute the sum of the risk values of the basin low points.
basinRiskSum :: UArray Coord Int -> BasinIds -> Int
basinRiskSum heights basinIds = sum [1+h | (c, h) <- assocs heights, Just c == basinIds!c]

-- | List of the sizes of all the basins on the grid.
basinSizes :: BasinIds -> [Int]
basinSizes = toList . counts . catMaybes . elems

-- * List utilities

-- | Returns the @n@ largest elements of a list
top :: Ord a => Int -> [a] -> [a]
top n = take n . sortBy (flip compare)
