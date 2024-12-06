{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/6>

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
import Advent (getInputArray, arrIx, ordNub)
import Advent.Coord (Coord, north, turnRight)
import Data.Array.Unboxed (UArray, (//), assocs)
import Data.Set qualified as Set

-- | >>> :main
-- 5239
-- 1753
main :: IO ()
main =
 do input <- getInputArray 2024 6
    let start = head [p | (p, '^') <- assocs input]
    let path1 = ordNub (map snd (walk input north start))
    print (length path1)
    print (length [() | p <- drop 1 path1 -- only bother checking the the initial path
                       , let input' = input // [(p,'#')] -- place a wall
                       , isLoop (walk input' north start)])

-- | Generate the list of directions and positions generated walking from the
-- starting point.
walk ::
    UArray Coord Char {- ^ input map                       -} ->
    Coord             {- ^ direction                       -} ->
    Coord             {- ^ position                        -} ->
    [(Coord, Coord)]  {- ^ list of direction and positions -}
walk grid d p =
    case arrIx grid (d + p) of
        Nothing  -> [(d, p)]                    -- fell off
        Just '#' -> walk grid (turnRight d) p   -- hit wall
        _        -> (d,p) : walk grid d (d + p) -- open space

-- | Predicate for paths that loop instead of running off the edge of the map.
isLoop :: Ord a => [a] -> Bool
isLoop = go Set.empty
  where
   go seen (x:xs) = Set.member x seen || go (Set.insert x seen) xs
   go _ [] = False
