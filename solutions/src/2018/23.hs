{-# Language ViewPatterns, ImportQualifiedPost, DataKinds, GADTs, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/23>

Part 2 enumerates maxmimal cliques and checks them for optimality.

-}
module Main (main) where

import Advent (countBy, format)
import Advent.Coord3 (manhattan, Coord3(..), origin)
import Advent.MaxClique (maxCliques)
import Data.List (maximumBy)
import Data.Ord (comparing)

data Bot = Bot { botPos :: !Coord3, botRadius :: !Int }
  deriving (Eq, Ord, Show)

-- | Predicate for points that a bot's signal is strong enough to reach.
-- These points form a regular octohedron centered at the bot's location.
botSees :: Bot -> Coord3 -> Bool
botSees (Bot c r) p = manhattan c p <= r

-- | Print the answers to day 23
--
-- >>> :main
-- 219
-- 83779034
main :: IO ()
main =
 do inp <- [format|2018 23 (pos=<%d,%d,%d>, r=%d%n)*|]
    let bots = [Bot (C3 x y z) r | (x,y,z,r) <- inp]
    print (part1 bots)
    print (part2 bots)

-- | Compute the number of nanobots (including self) that are in
-- range of the nanobot with the largest radius.
part1 :: [Bot] -> Int
part1 bots = countBy (botSees strongBot . botPos) bots
  where
    strongBot = maximumBy (comparing botRadius) bots

-- | Compute the minimum distance to the point that is in range of the
-- maximum number of nanobots.
--
-- To find this region we enumerate the maximal cliques of the graph and compare
-- each to find the ones with the largest clique size then with the minimal distance
-- to the origin.
--
-- >>> part2 [Dim 0 6 Pt, Dim 5 8 Pt, Dim 6 8 Pt]
-- 5
--
-- >>> part2 [Dim 0 6 Pt, Dim 5 8 Pt, Dim (-8) (-3) Pt, Dim (-7) (-2) Pt]
-- 4
part2 :: [Bot] -> Int
part2 = snd . minimum . map characterize . maxCliques botOverlap
  where
    characterize bs = (- length bs, maximum (map distToOrigin bs))

-- | Check if two bots have sensor range overlap.
botOverlap :: Bot -> Bot -> Bool
botOverlap (Bot c1 r1) (Bot c2 r2) = manhattan c1 c2 < r1 + r2

-- | Minimum distance from edge of bot's range to the origin (or zero if overlapping).
distToOrigin :: Bot -> Int
distToOrigin (Bot c r) = max 0 (manhattan origin c - r)
