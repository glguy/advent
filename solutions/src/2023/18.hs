{-# Language QuasiQuotes, LambdaCase, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/18>

Computes the area of the described polynomial using the
<https://en.wikipedia.org/wiki/Shoelace_formula>.

The extra perimeter factor accounts for the path that is being described as
having its own area. Each unit of the path is a 1x1 square. There will always
been as many top and left perimeter edges as bottom and right edges, but only
(for example) the top and left edges will be contained in the polygon. Half the
perimeter accounts for the squares that hang off the bottom and right. An extra
one unit accounts for the missed square at the bottom-right of the polygon.

>>> :{
:main +
"R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"
:}
62
952408144115

-}
module Main (main) where

import Advent (format, stageTH, partialSums)
import Advent.Coord (Coord(..), east, north, south, west, scaleCoord, norm1)
import Data.List (tails)

data D = DD | DU | DR | DL

stageTH

-- | Parse the input and print the answers to both parts.
--
-- >>> :main
-- 41019
-- 96116995735219
main :: IO ()
main =
 do input <- [format|2023 18 (@D %d %(#%x%)%n)*|]
    print (area [scaleCoord n (asUnitVec d) | (d,n,_) <- input])
    print (area [scaleCoord n ([east, south, west, north] !! d) | (_,_,x) <- input, let (n,d) = x `quotRem` 16])

-- | Computes the area of a path including the 1x1 square of the boundary.
-- The path is given as a list of cardinal direction movement vectors that
-- make up the trench instructions.
area :: [Coord] -> Int
area input = abs (polyareaRect path) + perimeter `quot` 2 + 1
  where
    path      = partialSums input
    perimeter = sum (map norm1 input)

-- | Convert the input character to a unit vector.
asUnitVec :: D -> Coord
asUnitVec = \case
  DR -> east
  DD -> south
  DL -> west
  DU -> north

-- | Area of a polygon using Shoelace formula over a closed loop.
polyareaRect :: [Coord] -> Int
polyareaRect xs = sum [x1 * y2 - x2 * y1 | C y1 x1 : C y2 x2 : _ <- tails xs] `quot` 2
