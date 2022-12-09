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
    let knots = infiniteRope (concatMap expand input)
    print (length (Set.fromList (knots !! 1)))
    print (length (Set.fromList (knots !! 9)))

-- | Generate a stream of lists, one for each knot in the rope.
-- Each list contains the coordinates of that knot at each time step.
infiniteRope :: [C] -> [[Coord]]
infiniteRope
  = iterate (scanl1 updateTail)  -- repeatedly apply tail update operation
  . scanl (+) origin             -- compute partial sums of steps
  . map cToVec                   -- convert input commands to step vectors

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

-- | Compute the next tail position given an updated head position.
updateTail ::
  Coord {- ^ old tail -} ->
  Coord {- ^ new head -} ->
  Coord {- ^ new tail -} 
updateTail tailKnot headKnot
  | isNearOrigin delta = tailKnot
  | otherwise          = tailKnot + signum delta
  where
    delta = headKnot - tailKnot

-- | Predicate for coordinates at or adjacent to the origin.
--
-- >>> all isNearOrigin [C y x | y <- [-1..1], x <- [-1..1]]
-- True
--
-- >>> any isNearOrigin [C 2 0, C 0 2, C 2 1, C (-2) 0, C (-1) 2]
-- False
isNearOrigin :: Coord -> Bool
isNearOrigin (C y x) = abs x < 2 && abs y < 2
