{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/24>

>>> :{
:main +
  "#.######\n\
  \#>>.<^<#\n\
  \#.<..<<#\n\
  \#>v.><>#\n\
  \#<^v^^>#\n\
  \######.#\n"
:}
18
54

-}
module Main where

import Data.Ix (inRange)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Advent ( getInputMap )
import Advent.Coord

-- |
-- >>> :main
-- 295
-- 851
main :: IO ()
main =
 do input <- Map.filter ('.' /=) <$> getInputMap 2022 24

    let corner = maximum (Map.keys input)
    let target = corner + west

    let loop t best land
          | Set.member land best = t
          | otherwise =
            loop (t+1) (grow corner world best) land
              where world = worldAt corner (t+1) input

    let t1 = loop 0 (Set.singleton (C 0 1)) target
        t2 = loop t1 (Set.singleton target) (C 0 1)
        t3 = loop t2 (Set.singleton (C 0 1)) target
    print t1
    print t3

-- | Find the set of obstructions in the world at a given time step
worldAt :: Coord -> Int -> Map Coord Char -> Set Coord
worldAt corner t input =
  Set.fromList [ location corner k t v | (k,v) <- Map.toList input]

-- | Given a set of locations the elf could be find the set the elf can be at next.
grow :: Coord -> Set Coord -> Set Coord -> Set Coord
grow corner world best =
  Set.fromList
    [ next
      | here <- Set.toList best
      , next@(C y x) <- here : cardinal here
      , inRange (0,corner) next
      , Set.notMember next world
    ]

dir :: Char -> Coord
dir '>' = east
dir '<' = west
dir '^' = north
dir 'v' = south
dir _ = undefined

-- | Compute the location an obstacle will be at at a given time step
location :: Coord -> Coord -> Int -> Char -> Coord
location _ here _ '#' = here
location corner here t c =
  zipCoord mod (here - 1 + scaleCoord t (dir c)) (corner - 1) + 1

