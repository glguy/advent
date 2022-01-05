{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/9>

-}
module Main (main) where

import Advent.Format (format)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as V

-- |
-- >>> :main
-- 18272118
-- 2186361
main :: IO ()
main =
  do inp <- [format|2020 9 (%d%n)*|]
     let v = V.fromList inp

     let target = part1 v
     print target

     let range = part2 v 0 target
     print (V.minimum range + V.maximum range)

-- | Returns 'True' when no pair of numbers exists in the vector that
-- sums up to the given target value.
search :: Int -> Vector Int -> Bool
search target haystack = and
  [ haystack V.! i + haystack V.! j /= target
  | i <- [0   .. V.length haystack - 2]
  , j <- [i+1 .. V.length haystack - 1]
  ]

-- | Find a number in the vector that is /not/ the sum of any pair of
-- numbers in the 25 elements preceding it.
part1 :: Vector Int -> Int
part1 v
  | search (v V.! 25) (V.take 25 v) = v V.! 25
  | otherwise = part1 (V.tail v)

-- | Find a contiguous subsequence of the given vector that sums to
-- the given target.
part2 ::
  Vector Int {- ^ remaining elements                -} ->
  Int         {- ^ leading elements used             -} ->
  Int         {- ^ remaining target value            -} ->
  Vector Int {- ^ subsequence matching target value -}
part2 v n acc =
  case compare acc 0 of
    EQ | n > 1 -> V.take n v                              -- done
    GT         -> part2 v          (n+1) (acc - v V.! n)  -- use another element
    _          -> part2 (V.tail v) (n-1) (acc + V.head v) -- stop using an element
