{-# Language ViewPatterns #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/6>

A guard walks around a grid. We check to see how long his route is and
how many ways we can add an obstacle that will turn his route into a
loop.

>>> :{
:main + "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
"
:}
41
6

-}
module Main (main) where

import Advent (getInputArray, countBy, ordNub)
import Advent.Coord (Coord, north, turnRight, charToVec)
import Data.Array.Unboxed (UArray, (//), (!?), assocs, amap)

-- | >>> :main
-- 5239
-- 1753
main :: IO ()
main =
 do input <- getInputArray 2024 6
    let (dir, start) = head [(dir, p) | (p, charToVec -> Just dir) <- assocs input]
        walls = amap ('#' ==) input
        path1 = ordNub (map snd (walk walls dir start))
        check2 p = isLoop (walk (walls // [(p, True)]) dir start)
    print (length path1)
    print (countBy check2 (drop 1 path1))

-- | Generate the list of directions and positions generated walking from the
-- starting point.
walk ::
    UArray Coord Bool {- ^ wall map                        -} ->
    Coord             {- ^ direction                       -} ->
    Coord             {- ^ position                        -} ->
    [(Coord, Coord)]  {- ^ list of direction and positions -}
walk grid d p =
    (d, p) :
    case grid !? (d + p) of
        Nothing    -> []                        -- fell off
        Just True  -> walk grid (turnRight d) p -- hit wall
        Just False -> walk grid d (d + p)       -- moved

-- | Predicate for paths that loop instead of running off the edge of the map.
-- <https://en.wikipedia.org/wiki/Cycle_detection#Floyd's_tortoise_and_hare>
isLoop :: Eq a => [a] -> Bool
isLoop a = go a a
  where
   go (x:xs) (_:y:ys) = x == y || go xs ys
   go _      _        = False
