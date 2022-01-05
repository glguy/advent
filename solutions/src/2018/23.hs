{-# Language ViewPatterns, ImportQualifiedPost, DataKinds, GADTs, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/23>

-}
module Main (main) where

import Advent (countBy, format)
import Advent.Box (Box(..), intersectBoxes, intersectBox, unionBoxes)
import Advent.Coord3 (manhattan, Coord3(..))
import Advent.Nat (Nat(S, Z))
import Data.List (maximumBy, foldl1')
import Data.Map qualified as Map
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
-- The region with the most intersections must contain one of the corners
-- of the nanobot regions, so take the intersection of all nanobots that
-- can see each of those corners, prioritize to only consider those intersections
-- comprised of the maximum number of nanobots, and then take the minimum
-- distance to the origin for any of those regions.
part2 :: [Bot] -> Int
part2 bots = distToOrigin maxRegion
  where
    boxes          = map botBox bots
    p              = searchBoxInMaxOverlap boxes
    Just maxRegion = intersectBoxes [b | b <- boxes, isJust (intersectBox b p)]

-- | Compute the minimum distance of any point contained within a box to the origin.
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

-- | Find a box that is completely contained within the maximally overlapping region.
searchBoxInMaxOverlap :: [Box n] -> Box n
searchBoxInMaxOverlap boxes = go [unionBoxes boxes]
  where
    -- If the only candidate is completely contained within the boxes
    -- with which it overlaps, then the search is complete.
    go [x] | all (maybe True (x==) . intersectBox x) boxes = x

    go candidates = go $ Map.elems $ snd $ foldl1' merge
      [ (length touches, Map.singleton touches s)
        | s <- splitBox =<< candidates
        , let touches = [b | b <- boxes, isJust (intersectBox b s) ]
      ]
      
    merge (n1,m1) (n2,m2)
      | n1 > n2 = (n1,m1)
      | n1 < n2 = (n2,m2)
      | otherwise = (n1, Map.union m1 m2)

-- | Split a box along each of its axes unless that axis is only one unit wide.
splitBox :: Box n -> [Box n]
splitBox Pt = [Pt]
splitBox (Dim lo hi x) =
 do y <- splitBox x
    if lo + 1 == hi then
      [Dim lo hi y]
    else
     do let m = lo + (hi-lo)`div`2
        [Dim lo m y, Dim m hi y]
