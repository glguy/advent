{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/9>

-}
module Main where

import Advent (format, stageTH)
import Advent.Coord (Coord(..), origin, east, north, south, west)
import Data.List (transpose)
import Data.Set qualified as Set

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
main = do
    input <- [format|2022 9 (@C %u%n)*|]
    let knots = uniqueLocations (concatMap expand input)
    print (knots !! 1)
    print (knots !! 9)

-- | Generate the number of unique locations each knot in an infinitely long
-- rope visits give a list of step commands
uniqueLocations :: [C] -> [Int]
uniqueLocations = map countUnique . transpose . scanl stepRope (repeat origin)

-- | Return the number of unique elements in a list.
countUnique :: Ord a => [a] -> Int
countUnique = length . Set.fromList

-- | Replicate the first element second element number of times.
--
-- >>> expand ('a', 5)
-- "aaaaa"
expand :: (a, Int) -> [a]
expand (x,n) = replicate n x

-- | Generate the unit vector corresponding to an input command.
cToVec :: C -> Coord
cToVec CU = north
cToVec CD = south
cToVec CR = east
cToVec CL = west

-- | Predicate for coordinates at or adjacent to the origin.
--
-- >>> all isNearOrigin [C y x | y <- [-1..1], x <- [-1..1]]
-- True
--
-- >>> any isNearOrigin [C 2 0, C 0 2, C 2 1, C (-2) 0, C (-1) 2]
-- False
isNearOrigin :: Coord -> Bool
isNearOrigin (C y x) = abs x < 2 && abs y < 2

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
