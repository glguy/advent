{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/4>

-}
module Main where

import Advent ( format, countBy )

-- |
-- > :main
-- 584
-- 933
main :: IO ()
main =
 do input <- [format|2022 4 (%u-%u,%u-%u%n)*|]
    print $ countBy (\(a,b,c,d) -> subset (a,b) (c,d) || subset (c,d) (a,b)) input
    print $ countBy (\(a,b,c,d) -> inRange (a,b) c || inRange (a,b) d || inRange (c,d) a || inRange (c,d) b) input

subset :: Ord a => (a, a) -> (a, a) -> Bool
subset (a,b) (c,d) = c <= a && b <= d

inRange :: Ord a => (a, a) -> a -> Bool
inRange (a,b) x = a <= x && x <= b
