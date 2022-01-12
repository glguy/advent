{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/3>

-}
module Main (main) where

import Advent.Format (format)
import Advent.Coord
import Control.Applicative (liftA2)
import Data.Foldable (asum)
import Data.List (foldl1')
import Data.Map (Map)
import Data.Map qualified as Map

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> let parse = [format|- ((@D%u)&,%n)*|] . unlines

-- | Directions up, down, left, and right.
data D = DU | DD | DL | DR
  deriving Show

mempty

-- coordinates ---------------------------------------------------------

-- | Convert a direction letter unit vector in the given direction.
toUnitVector :: D -> Coord
toUnitVector DU = north
toUnitVector DD = south
toUnitVector DL = west
toUnitVector DR = east

------------------------------------------------------------------------

-- | >>> :main
-- 2129
-- 134662
main :: IO ()
main =
  do (p1,p2) <- answers <$> [format|2019 3 ((@D%u)&,%n)*|]
     print p1
     print p2

-- | Given the input file parsed as lists of lists of motions, compute the
-- nearest distance to origin and minimum sum steps to intersection.
--
-- >>> let check = answers . parse
-- >>> check ["R8,U5,L5,D3","U7,R6,D4,L4"]
-- (6,30)
-- >>> check ["R75,D30,R83,U83,L12,D49,R71,U7,L72","U62,R66,U55,R34,D71,R55,D58,R83"]
-- (159,610)
-- >>> check ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51","U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
-- (135,410)
answers :: [[(D, Int)]] -> (Int, Int)
answers xs = (nearestDistanceToOrigin intersections, minimum intersections)
  where
    intersections = pathIntersections xs

-- | Computes the map of path intersections, compute the shortest
-- distance of an intersection to the origin.
nearestDistanceToOrigin :: Map Coord a -> Int
nearestDistanceToOrigin = minimum . map (manhattan origin) . Map.keys

-- | Given a list of paths compute a map of locations that have intersections
-- among all of the paths. The value at each location is the sum of the
-- number of steps taken along each of the paths to get to that point.
--
-- >>> let check = pathIntersections . parse
-- >>> check ["R8,U5,L5,D3","U7,R6,D4,L4"]
-- fromList [(C (-5) 6,30),(C (-3) 3,40)]
pathIntersections :: [[(D,Int)]] -> Map Coord Int
pathIntersections = foldl1' (Map.intersectionWith (+)) . map distances

-- | Generate a map of the coordinates a path visits. Each coordinate is
-- indexed by the number of steps it took to get to that location.
--
-- >>> distances [(DD,2), (DR,1)]
-- fromList [(C 1 0,1),(C 2 0,2),(C 2 1,3)]
distances :: [(D,Int)] -> Map Coord Int
distances steps = Map.fromListWith min (zip (generatePath steps) [1..])

-- | Generate the list of coordinates visited by a list of steps.
--
-- >>> generatePath [(DD,2), (DR,1)]
-- [C 1 0,C 2 0,C 2 1]
generatePath :: [(D,Int)] -> [Coord]
generatePath
  = scanl1 (+)
  . concatMap (\(d,n) -> replicate n (toUnitVector d))
