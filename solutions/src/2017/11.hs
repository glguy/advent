{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/11>

Day 11 asks us to implement a hex grid coordinate system
and compute distances on it.

<https://www.redblobgames.com/grids/hexagons/>

* X grid lines are diagonal from @sw@ to @ne@
* Y grid lines are vertical

@
  +  1,2 +
   \\    /
0,2 +--+  2,1
   /    \\
 -+  1,1 +-
   \\    /
0,1 +--+  2,0
   /    \\
  +  1,0 +
@

-}
module Main where

import Advent.Format (format)
import Advent.Coord (Coord(..), north, east, south, west)
import Data.List (scanl')

data D = Dn | Dne | Dnw | Dse | Dsw | Ds deriving Show

mempty

-- | Print the solutions to day 11. The input file can be overridden
-- via the command-line.
--
-- >>> :main
-- 761
-- 1542
main :: IO ()
main =
  do input <- [format|2017 11 @D&,%n|]
     let distances = map distance (partialSums (map translate input))
     print (last    distances)
     print (maximum distances)

-- | Compute minimum path distance from the origin on the hex grid.
--
-- >>> distance <$> [C (-1) 0,C (-1) 1,C 0 (-1),C 0 1,C 1 (-1),C 1 0]
-- [1,1,1,1,1,1]
-- >>> distance <$> [C (-1) (-1),C 1 1,C 2 (-1)]
-- [2,2,2]
distance :: Coord -> Int
distance (C y x) = maximum (map abs [x,y,x+y])

-- | Translate hex direction to grid projection.
--
-- >>> translate <$> [Dn,Ds,Dne,Dse,Dnw,Dsw]
-- [C (-1) 0,C 1 0,C (-1) 1,C 0 1,C 0 (-1),C 1 (-1)]
translate :: D -> Coord
translate Dne = north + east
translate Dn  = north
translate Dnw = west
translate Dsw = south + west
translate Ds  = south
translate Dse = east

-- | Compute the partial sums of a list.
--
-- >>> partialSums [1,4,7]
-- [0,1,5,12]
partialSums :: Num a => [a] -> [a]
partialSums = scanl' (+) 0