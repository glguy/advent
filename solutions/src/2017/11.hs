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
import Advent.Coord (Coord(..), above, below, left, right, origin)

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
  do input <- [format|11 @D&,%n|]
     let distances = distance <$> scanl (flip move) origin input
     print (last    distances)
     print (maximum distances)


-- | Compute minimum path distance from the origin on the hex grid.
--
-- >>> distance <$> [C (-1) 0,C (-1) 1,C 0 (-1),C 0 1,C 1 (-1),C 1 0)]
-- [1,1,1,1,1,1]
-- >>> distance <$> [C (-1) (-1),C 1 1,C 2 (-1)]
-- [2,2,2]
distance :: Coord -> Int
distance (C y x) = maximum (map abs [x,y,x+y])

-- | Move one cell on the hex grid.
--
-- >>> (`move` origin) <$> [Dn,Ds,Dne,Dse,Dnw,Dsw]
-- [C 1 0,C (-1) 0,C 0 1,C (-1) 1,C 1 (-1),C 0 (-1)]
move :: D -> Coord -> Coord
move Dn  = below
move Ds  = above
move Dne = right
move Dsw = left
move Dnw = left . below
move Dse = right . above
