{-# Language ImportQualifiedPost, BangPatterns #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/22>

-}
module Main where

import Advent.Coord (Coord(..), turnLeft, turnRight, turnAround, north)
import Advent.Input (getInputMap)
import Control.Lens
import Data.Map (Map)
import Data.Map qualified as Map


main :: IO ()
main =
  do input <- getInputMap 22
     let grid = Map.mapMaybe (\x -> if x == '#' then Just Infected else Nothing) input
     let C my mx = last (Map.keys input)
     let start = C (my `div` 2) (mx `div` 2)
     print (go rule1    10000 0 north start grid)
     print (go rule2 10000000 0 north start grid)


data Status = Clean | Weakened | Infected | Flagged deriving Eq


-- | Transition rule used in part 1
rule1 :: Status -> Status
rule1 Clean = Infected
rule1 _     = Clean

-- | Transition rule used in part 2
rule2 :: Status -> Status
rule2 Clean    = Weakened
rule2 Weakened = Infected
rule2 Infected = Flagged
rule2 Flagged  = Clean

-- | Turn rule used by the virus carrier.
turnRule :: Status -> Coord -> Coord
turnRule Clean    = turnLeft
turnRule Weakened = id
turnRule Infected = turnRight
turnRule Flagged  = turnAround


-- | Compute the starting position and world map from the input file.
parseInput :: String -> (Coord, Map Coord Status)
parseInput str = (start, world)
  where
    rows = reverse (lines str)

    start = C (length (head rows) `div` 2) (length rows `div` 2)
    world = Map.fromList
          $ map (\(y,x) -> (C x y, Infected))
          $ toListOf ( (folded <.> folded) . only '#' . asIndex) rows


-- | Run the world simulation for a specified number of iterations.
-- Returns the number of infections caused by the virus carrier.
go ::
  (Status -> Status) {- ^ update rule                              -} ->
  Int                {- ^ iterations remaining                     -} ->
  Int                {- ^ infection counter                        -} ->
  Coord              {- ^ facing direction                         -} ->
  Coord              {- ^ current location                         -} ->
  Map Coord Status   {- ^ world map                                -} ->
  Int                {- ^ infections caused after given iterations -}
go _    0 !acc !_   !_ !_     = acc
go rule n !acc !dir !c !world = go rule n' acc' dir' c' world'
  where
    n'     = n - 1

    cell   = view loc world
    cell'  = rule cell
    world' = set loc cell' world

    acc' | cell' == Infected = 1 + acc
         | otherwise         = acc

    dir' = turnRule cell dir  -- new facing direction
    c'   = c + dir'           -- new world coordinate

    loc :: Lens' (Map Coord Status) Status
    loc = at c . non Clean
