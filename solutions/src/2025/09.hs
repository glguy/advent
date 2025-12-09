{-# Language QuasiQuotes, DataKinds, GADTs #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/9>

>>> :{
:main +
"7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
"
:}
50
24

-}
module Main (main) where

import Advent ( format )
import Advent.Box ( coverBoxes, intersectBox, size, subtractBox, unionBoxes, Box(Pt, Dim), Box' )
import Advent.Coord ( Coord(..) )
import Data.List ( delete, partition, minimumBy, sortBy, tails )
import Data.Maybe ( isNothing )
import Data.Ord ( comparing )

-- | >>> :main
-- 4767418746
-- 1461987144
main :: IO ()
main =
 do input <- [format|2025 9 (%u,%u%n)*|]
    let coords = [C y x | (x, y) <- input]

    -- Candidate rectangles sorted by size
    let rectangles = sortBy (flip (comparing size)) [asRect x y | x:ys <- tails coords, y <- ys]

    -- Part 1 just wants the largest rectangle
    print (size (head rectangles))

    -- The region given in the input file
    let strips = unionBoxes [asRect x y | x:y:_ <- tails (coords ++ [head coords])]

    -- A rectangle that's a little bigger than the whole region in the input file
    let outer = grow (coverBoxes strips)

    -- The outer rectangle with the input region subtracted
    let shadow = foldl (\acc x -> concatMap (subtractBox x) acc) [outer] strips

    -- A box that is definitely outside the diagram (top-left corner)
    let start = minimum shadow

    -- The region that our target rectangle must not touch
    let outOfBounds = fill [start] (delete start shadow)

    -- A valid rectangle for part 2 is not touching any of the out of bounds region
    let valid x = all (isNothing . intersectBox x) outOfBounds

    -- Part 2 is the largest rectangle not touching the out of bound region
    print (size (head (filter valid rectangles)))

-- | Construct the box given the coordinates of two opposite corners
asRect :: Coord -> Coord -> Box' 2
asRect (C y1 x1) (C y2 x2) = Dim (min x1 x2) (max x1 x2 + 1) (Dim (min y1 y2) (max y1 y2 + 1) Pt)

-- | Expand a box by one in all directions
grow :: Box' 2 -> Box' 2
grow (Dim x1 x2 (Dim y1 y2 Pt)) = Dim (x1 - 1) (x2 + 1) (Dim (y1 - 1) (y2 + 1) Pt)

-- | Given a list of selected boxes and a list of available boxes, returns the list of
-- contiguous boxes.
fill :: [Box' 2] -> [Box' 2] -> [Box' 2]
fill [] _ = []
fill (x:xs) available = x : fill (touched ++ xs) available'
  where
    (touched, available') = partition (adjacent x) available

-- | Predicate for disjoint, touching boxes.
adjacent :: Box' 2 -> Box' 2 -> Bool
adjacent (Dim xlo1 xhi1 (Dim ylo1 yhi1 Pt)) (Dim xlo2 xhi2 (Dim ylo2 yhi2 Pt))
  | xhi1 == xlo2 = max ylo1 ylo2 < min yhi1 yhi2
  | xhi2 == xlo1 = max ylo1 ylo2 < min yhi1 yhi2
  | yhi1 == ylo2 = max xlo1 xlo2 < min xhi1 xhi2
  | yhi2 == ylo1 = max xlo1 xlo2 < min xhi1 xhi2
  | otherwise = False
