{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/9>

>>> :{
:main +
  "R 4\n\
  \U 4\n\
  \L 3\n\
  \D 1\n\
  \R 4\n\
  \D 1\n\
  \L 5\n\
  \R 2\n"
:}
13
1

>>> :{
:main +
  "R 5\n\
  \U 8\n\
  \L 8\n\
  \D 3\n\
  \R 17\n\
  \D 10\n\
  \L 25\n\
  \U 20\n"
:}
88
36

>>> uniqueLocations [(CR,4),(CU,4),(CL,3),(CD,1),(CR,4),(CD,1),(CL,5),(CR,2)] !! 1
13

>>> uniqueLocations [(CR,5),(CU,8),(CL,8),(CD,3),(CR,17),(CD,10),(CL,25),(CU,20)] !! 9
36

-}
module Main where

import Data.List (transpose)
import Data.Set qualified as Set

import Advent (format, stageTH)
import Advent.Coord (Coord(..), origin, east, north, south, west, normInf)

-- | Rope movement instructions
data C
  = CD -- ^ move down
  | CR -- ^ move right
  | CU -- ^ move up
  | CL -- ^ move left
  deriving Show

stageTH

-- | Print the answers to both parts of day 9. Automatically finds
-- input file unless overridden with a command line argument.
--
-- >>> :main
-- 5930
-- 2443
main :: IO ()
main =
 do input <- [format|2022 9 (@C %u%n)*|]
    let knots = uniqueLocations input
    print (knots !! 1)
    print (knots !! 9)

-- | Generate the number of unique locations each knot in an infinitely long
-- rope visits given a list of movement commands.
uniqueLocations :: [(C,Int)] -> [Int]
uniqueLocations
  = map countUnique                -- list of unique locations per knot
  . transpose                      -- list of steps to list of knots
  . scanl stepRope (repeat origin) -- step a rope starting at origin through list of movements
  . concatMap (\(c,n) -> replicate n c)

-- | Generate the unit vector corresponding to an input command.
cToVec :: C -> Coord
cToVec CU = north
cToVec CD = south
cToVec CR = east
cToVec CL = west

-- | Update all the knot locations in a rope given a step direction for the head knot.
stepRope ::
  [Coord] {- ^ knot locations         -} ->
  C       {- ^ next step direction    -} ->
  [Coord] {- ^ updated knot locations -}
stepRope (x:xs) c = updateTails (cToVec c + x) xs
stepRope []     _ = []

-- | Update all the tail knots in the rope given a new head position.
updateTails ::
  Coord   {- ^ head         -} ->
  [Coord] {- ^ tails        -} ->
  [Coord] {- ^ updated rope -}
updateTails h [] = [h]
updateTails h (t : ts)
  | isNearOrigin delta = h : t : ts -- once a knot is stationary, the rest will be, too
  | otherwise          = h : updateTails (t + signum delta) ts
  where
    delta = h - t

-- | Predicate for coordinates at or adjacent to the origin.
--
-- >>> all isNearOrigin [C y x | y <- [-1..1], x <- [-1..1]]
-- True
--
-- >>> any isNearOrigin [C 2 0, C 0 2, C 2 1, C (-2) 0, C (-1) 2]
-- False
isNearOrigin :: Coord -> Bool
isNearOrigin c = normInf c < 2

-- | Return the number of unique elements in a list.
countUnique :: Ord a => [a] -> Int
countUnique = length . Set.fromList
