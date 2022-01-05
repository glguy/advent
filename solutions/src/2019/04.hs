{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/4>

-}
module Main (main) where

import Advent (format, countBy)
import Data.List (group)

-- | >>> :main
-- 1929
-- 1306
main :: IO ()
main =
  do [(lo,hi)] <- [format|2019 4 (%u-%u%n)*|]
     let nums = map runs $ filter nondecreasing $ map show [lo..hi]
     print (countBy (any (> 1)) nums)
     print (countBy (elem 2   ) nums)

-- | Return a list of the lengths of consecutive elements in a list.
--
-- >>> runs [1,2,3]
-- [1,1,1]
-- >>> runs [1,1,1]
-- [3]
-- >>> runs [1,1,2,2,2,1,1]
-- [2,3,2]
-- >>> runs []
-- []
runs :: Eq a => [a] -> [Int]
runs = map length . group

-- | Predicate for non-decreasing lists.
--
-- >>> nondecreasing []
-- True
-- >>> nondecreasing [1,1,2,3]
-- True
-- >>> nondecreasing [3,3,2]
-- False
nondecreasing :: Ord a => [a] -> Bool
nondecreasing xs = and (zipWith (<=) xs (tail xs))
