{-# Language DataKinds, GADTs, QuasiQuotes #-}
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
import Advent.Box (Box(..), intersectBox)
import Advent.Coord3 (manhattan, origin, Coord3(..))
import Advent.Nat (Nat(S, Z))
import Data.List (maximumBy)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

data Bot = Bot !Coord3 !Int
  deriving (Eq, Ord, Show)

-- | Print the answers to day 23
--
-- >>> :main
-- 219
-- 83779034
main :: IO ()
main =
 do inp <- [format|23 (pos=<%d,%d,%d>, r=%d%n)*|]
    let bots = [Bot (C3 x y z) r | (x,y,z,r) <- inp]
    print (part1 bots)
    print (part2 bots)

-- | Compute the maximum number of nanobots (including self) that are in
-- range of any nanobot.
part1 :: [Bot] -> Int
part1 bots = countBy (\(Bot bot _) -> manhattan bot here <= r) bots
  where
    Bot here r = maximumBy (comparing (\(Bot _ r) -> r)) bots

-- | Compute the minimum distance to the point that is in range of the
-- maximum number of nanobots.
--
-- The region with the most intersections must contain one of the corners
-- of the nanobot regions, so take the intersection of all nanobots that
-- can see each of those corners, prioritize to only consider those intersections
-- comprised of the maximum number of nanobots, and then take the minimum
-- distance to the origin for any of those regions.
part2 :: [Bot] -> Int
part2 bots
  = snd $ minimum
    [(-length bots', distToOrigin region)
    | bot <- bots
    , p   <- corners bot
    , let bots' = filter (\(Bot botPos botRad) -> manhattan p botPos <= botRad) bots
    , let region = foldl1 (\acc x -> fromJust (intersectBox acc x)) (map botBox bots')
    ]

-- | Compute the minimum distance of any point contained within a box to the origin.
distToOrigin :: Box n -> Int
distToOrigin Pt = 0
distToOrigin (Dim lo hi xs) = here `max` distToOrigin xs
  where
    here
      | hi <= 0 = 1-hi
      | 0 <= lo = lo
      | otherwise = 0

-- | Find all the extremes of the octohedron around a bot.
corners :: Bot -> [Coord3]
corners (Bot (C3 x y z) r) =
  [ C3 (x+r) y z
  , C3 (x-r) y z
  , C3 x (y+r) z
  , C3 x (y-r) z
  , C3 x y (z+r)
  , C3 x y (z-r)
  ]

botBox :: Bot -> Box ('S ('S ('S ('S 'Z))))
botBox (Bot (C3 x y z) r) = dim cx (dim cy (dim cz (dim cw Pt)))
  where
    dim :: Int -> Box n -> Box ('S n)
    dim c = Dim (c - r) (c + r + 1)
    cx = x + y + z
    cy = x + y - z
    cz = x - y - z
    cw = x - y + z
