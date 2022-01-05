{-# Language ViewPatterns, ImportQualifiedPost, DataKinds, GADTs, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/23>

This solution translates the bot octahedrons into 4D cuboids where each parallel set
of faces of the octahedron translates to a pair of faces on a cube.

Part 2 enumerates maxmimal cliques and checks them for optimality.

-}
module Main (main) where

import Advent (countBy, format)
import Advent.Box (Box(..), intersectBox, intersectBoxes)
import Advent.Coord3 (manhattan, Coord3(..))
import Advent.MaxClique (maxCliques)
import Advent.Nat (Nat(S, Z))
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Maybe (isJust)
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
    print (part2 (map botBox bots))

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
part2 :: [Box n] -> Int
part2 = snd . minimum . map characterize . maxCliques touching
  where
    touching x y = isJust (intersectBox x y)
    characterize bs = (- length bs, distToOrigin (fromJust (intersectBoxes bs)))

-- | Compute the minimum radius of a hypercube at the origin that intersects
-- with the given box.
distToOrigin :: Box n -> Int
distToOrigin Pt = 0
distToOrigin (Dim lo hi xs) = here `max` distToOrigin xs
  where
    here
      | hi <= 0 = 1-hi
      | 0 <= lo = lo
      | otherwise = 0

-- | Translation of bot 3D center and radius into a 4D cube consisting of the four pairs
-- of two parallel planes that define an octahedron.
botBox :: Bot -> Box ('S ('S ('S ('S 'Z))))
botBox (Bot (C3 x y z) r) = dim cx (dim cy (dim cz (dim cw Pt)))
  where
    dim :: Int -> Box n -> Box ('S n)
    dim c = Dim (c - r) (c + r + 1)
    cx = x + y + z
    cy = x + y - z
    cz = x - y - z
    cw = x - y + z
