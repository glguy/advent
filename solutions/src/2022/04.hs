{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/4>

>>> :{
:main +
   "2-4,6-8\n\
   \2-3,4-5\n\
   \5-7,7-9\n\
   \2-8,3-7\n\
   \6-6,4-6\n\
   \2-6,4-8\n"
:}
2
4

-}
module Main where

import Advent (format, countBy)
import Data.Ix (inRange)

-- |
-- >>> :main
-- 584
-- 933
main :: IO ()
main =
 do input <- [format|2022 4 (%u-%u,%u-%u%n)*|]
    print $ countBy (\(a,b,c,d) -> a <= c && d <= b || c <= a && b <= d) input
    print $ countBy (\(a,b,c,d) -> inRange (a,b) c || inRange (a,b) d || inRange (c,d) a || inRange (c,d) b) input
