{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/9>

Find the basins on the height map.

-}
module Main (main) where

import Advent (counts, getInputArray, arrIx)
import Advent.Coord (Coord(..), cardinal)
import Data.Array.Unboxed (Array, UArray, (!), array, assocs, bounds, elems)
import Data.Char (digitToInt)
import Data.Foldable (toList)
import Data.List (sortBy)
import Data.Maybe (catMaybes)

type Heights  = Array Coord (Maybe Int)
type BasinIds = Array Coord (Maybe Coord)

-- >>> :main
-- 588
-- 964712
main :: IO ()
main =
 do heights <- heightArray <$> getInputArray 9
    let basinIds = heightsToBasinIds heights

    print (sum [1+h | (c, Just h) <- assocs heights, Just c == basinIds!c])
    print (product (top 3 (basinSizes basinIds)))

-- | Returns the @n@ largest elements of a list
top :: Ord a => Int -> [a] -> [a]
top n = take n . sortBy (flip compare)

-- | Convert that ASCII grid into a grid of heights
heightArray :: UArray Coord Char -> Heights
heightArray a = array (bounds a) [(c, cvt x) | (c, x) <- assocs a]
  where
    cvt '9' = Nothing -- 9s are defined to be out of bounds in the problem
    cvt x   = Just (digitToInt x)

heightsToBasinIds :: Heights -> BasinIds
heightsToBasinIds heights = basinIds
  where
    basinIds = array (bounds heights) [(c, basinId c =<< mbh) | (c, mbh) <- assocs heights]
    basinId c h =
     do xs <- sequence [basinIds!x | x <- cardinal c, Just (Just xh) <- [arrIx heights x], xh<h]
        case xs of
          []                  -> Just c -- this is the lowest point in the basin
          y:ys | all (y==) ys -> Just y -- all flows go to a unique basin
               | otherwise    -> Nothing -- no unique basin

basinSizes :: BasinIds -> [Int]
basinSizes = toList . counts . catMaybes . elems
