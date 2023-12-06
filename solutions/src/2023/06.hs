{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/6>

This problem asks us to consider the time we should spend
charging up a toy car to beat a target distance. The distance
the car will travel is a quadratic equation. What we end up
doing is finding the distance between the roots of the function.

-- >>> :{
:main +
"Time:      7  15   30
Distance:  9  40  200
"
:}
288
71503

-}
module Main where

import Advent (format)

-- |
--
-- >>> :main
-- 281600
-- 33875953
main :: IO ()
main =
 do (times, distances) <- [format|2023 6 Time:( +%s)*%nDistance:( +%s)*%n|]
    let input1 = zip (map read times) (map read distances)
        input2 = (read (concat times), read (concat distances))
    print (product (map ways input1))
    print (ways input2)

ways :: (Int, Int) -> Int
ways (t, d)
  | valid mid = hi - tooLo
  | otherwise = 0
  where
    valid hold = (t - hold) * hold > d
    mid = t `div` 2 -- the midpoint is the best we can get
    tooLo = binSearch (not . valid)   0 mid
    hi    = binSearch        valid  mid   t

binSearch ::
  (Int -> Bool) {- ^ predicate    -} ->
  Int           {- ^ small enough -} ->
  Int           {- ^ too big      -} ->
  Int
binSearch p lo hi
  | lo + 1 == hi = lo
  | p mid        = binSearch p mid hi
  | otherwise    = binSearch p lo mid
  where
    mid = lo + (hi - lo) `div` 2
