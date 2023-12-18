{-# Language QuasiQuotes, DataKinds, TemplateHaskell, ImportQualifiedPost, GADTs #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/18>

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

import Advent (format, stageTH)
import Advent.Box (coverBoxes, intersectBox, size, subtractBox, Box(Pt, Dim), Box')
import Advent.Coord (east, north, origin, scaleCoord, south, west, Coord(..))
import Control.Monad (foldM)
import Advent.Search (bfs)
import Data.Maybe (isJust)
import Numeric (readHex)

data D = DD | DU | DL | DR

stageTH

-- |
--
-- >>> :main
-- 41019
-- 96116995735219
main :: IO ()
main =
 do input <- [format|2023 18 (@D %d %(#%s%c%)%n)*|]
    let part1 = [(n, d) | (d,n,_,_) <- input]
    let part2 = [(fst (head (readHex str)), dir2 d) | (_,_,str,d) <- input]
    print (solve part1)
    print (solve part2)
    
solve :: [(Int, D)] -> Int
solve input = sum (map size ditches) + sum (map size reachable)
  where
    ditches = makePath origin input
    everything = coverBoxes ditches

    noDitch = foldM (flip subtractBox) everything ditches
    neighbors region = [x | x <- noDitch, touch x region]
    reachable = drop 1 (bfs neighbors (Dim 1 2 (Dim 1 2 Pt)))

touch :: Box' 2 -> Box' 2 -> Bool
touch (Dim y1 y2 (Dim x1 x2 Pt)) other =
  isJust (intersectBox (Dim (y1-1) (y2+1) (Dim (x1-1) (x2+1) Pt)) other)

dir2 :: Char -> D
dir2 '0' = DR
dir2 '1' = DD
dir2 '2' = DL
dir2 '3' = DU
dir2 _   = error "bad direction digit"

makePath :: Coord -> [(Int, D)] -> [Box' 2]
makePath _ [] = []
makePath here ((n, d) : xs) = toBox here n d : makePath (here + scaleCoord n (toVec d)) xs

toVec :: D -> Coord
toVec DD = south
toVec DL = west
toVec DR = east
toVec DU = north

toBox :: Coord -> Int -> D -> Box' 2
toBox (C y x) n DD = Dim y (y + n) (Dim x (x + 1) Pt)
toBox (C y x) n DR = Dim y (y + 1) (Dim x (x + n) Pt)
toBox (C y x) n DU = Dim (y - n + 1) (y+1) (Dim x (x + 1) Pt)
toBox (C y x) n DL = Dim y (y + 1) (Dim (x - n + 1) (x+1) Pt)
