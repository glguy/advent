{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost, TemplateHaskell, OverloadedLists #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/13>

Given a paper with some dots and a series of fold instructions
we fold and fold and fold and find our secret code.

-}
module Main (main) where

import Advent.Coord (Coord(C), drawCoords)
import Advent.Format (format)
import Data.Set (Set)
import Data.Set qualified as Set

data A = Ax | Ay deriving (Show)

mempty -- template haskell staging

-- | >>> :main
-- 716
-- ███  ███   ██  █  █ ████ ███  █    ███
-- █  █ █  █ █  █ █ █  █    █  █ █    █  █
-- █  █ █  █ █    ██   ███  ███  █    █  █
-- ███  ███  █    █ █  █    █  █ █    ███
-- █ █  █    █  █ █ █  █    █  █ █    █ █
-- █  █ █     ██  █  █ █    ███  ████ █  █
main :: IO ()
main =
 do (points, folds) <- [format|2021 13 (%u,%u%n)*%n(fold along @A=%u%n)*|]
    let pointSet = Set.fromList [C y x | (x, y) <- points]
        states   = scanl (flip foldPoints) pointSet folds
        p1       = states !! 1 -- points after first fold
        p2       = last states -- points after last fold
    print (length p1)
    putStr (drawCoords p2)

-- | 2-dimensional fold the set of points over a line.
foldPoints :: (A, Int) {- ^ fold line -} -> Set Coord -> Set Coord
foldPoints (Ax, lx) = Set.map \(C y x) -> C y (fold1 lx x)
foldPoints (Ay, ly) = Set.map \(C y x) -> C (fold1 ly y) x

-- | 1-dimensional fold updating one point
fold1 :: Int {- ^ fold -} -> Int {- ^ point -} -> Int
fold1 a i = a - abs (a - i)
