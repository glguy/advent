{-# Language QuasiQuotes, LambdaCase #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/18>

Computes the area of the described polynomial using the
<https://en.wikipedia.org/wiki/Shoelace_formula>.

The extra perimeter factor accounts for the path that
is being described as having its own area. Each unit of
the path is a 1x1 square. There will always been as many
top and left perimeter edges as bottom and right edges,
but only (for example) the top and left edges will be
contained in the polygon. Half the perimeter accounts for
the squares that hang off the bottom and right. An extra
one unit accounts for either the bottom-left or top-right
corner that gets excluded.

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

import Advent (format)
import Advent.Coord (east, north, origin, scaleCoord, south, west, Coord(..), norm1)
import Numeric (readHex)

-- | Parse the input and print the answers to both parts.
--
-- >>> :main
-- 41019
-- 96116995735219
main :: IO ()
main =
 do input <- [format|2023 18 (%c %d %(#%s%c%)%n)*|]
    print (solve [scaleCoord n (asUnitVec d)                        | (d,n,_,_) <- input])
    print (solve [scaleCoord (fst (head (readHex n))) (asUnitVec d) | (_,_,n,d) <- input])

solve :: [Coord] -> Int
solve input = abs (polyareaRect path) + perimeter `quot` 2 + 1
  where
    path      = scanl (+) origin input
    perimeter = sum [norm1 n | n <- input]

asUnitVec :: Char -> Coord
asUnitVec = \case 
  '0' -> east ; 'R' -> east
  '1' -> south; 'D' -> south 
  '2' -> west ; 'L' -> west 
  '3' -> north; 'U' -> north 
  _   -> error "bad direction digit"

-- | Area of a polygon using Shoelace formula
-- over a closed loop.
polyareaRect :: [Coord] -> Int
polyareaRect = f 0
  where
    f acc (C y1 x1 : xs@(C y2 x2 : _)) = f (acc + x1 * y2 - x2 * y1) xs
    f acc _ = acc `quot` 2
