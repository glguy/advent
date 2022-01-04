{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/10>

We're in an asteroid field of infinitesimal asteroids on a lattice.
We'll need to figure out which location can see the most other asteroids
and then figure out what order a clockwise sweep of them those asteroids
fall into.

-}
module Main (main) where

import Advent (getInputLines, countBy)
import Advent.Coord (coordLines, Coord(..), manhattan, scaleCoord)
import Data.Foldable (Foldable(toList))
import Data.List (sortOn, transpose)
import Data.Map qualified as Map
import Data.Ratio ((%))
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 227
-- 604
main :: IO ()
main =
 do inp <- getInputLines 10
    let locs = Set.fromList [c | (c,'#') <- coordLines inp]
    let (vis, base) = findBase locs
    print vis
    let C y x = spiral base (Set.delete base locs) !! 199
    print (x * 100 + y)

-- | Find the location of the base with the most visibility.
findBase :: Set Coord -> (Int, Coord)
findBase m = maximum [(countBy (lineOfSight m i) m, i) | i <- toList m]

-- | Return the coordinates ordered in a spiral given a center
-- location.
spiral ::
  Coord {- ^ center of spiral -} ->
  Set Coord {- ^ objects -} ->
  [Coord] {- ^ spiral ordered objects -}
spiral base
  = concat
  . transpose
  . map (sortOn (manhattan base))
  . groupOn (toAngle . subtract base)
  . Set.toList

-- | Check if two asteroids have line of sight between each other.
lineOfSight ::
  Set Coord {- ^ obstructions -} ->
  Coord -> Coord -> Bool
lineOfSight ast a b = a /= b && and [Set.notMember c ast | c <- between a b]

-- * Coordinate utilities

-- | Return all the locations that fall on a line between two coordinates.
--
-- >>> between (C 1 1) (C 7 4)
-- [C 3 2,C 5 3]
--
-- >>> between (C 1 1) (C 6 6)
-- [C 2 2,C 3 3,C 4 4,C 5 5]
--
-- >>> between (C 5 5) (C 1 1)
-- [C 4 4,C 3 3,C 2 2]
--
-- >>> between (C 1 1) (C 6 7)
-- []
--
-- >>> between (C 0 0) (C 0 0)
-- []
between :: Coord -> Coord -> [Coord]
between a b = [a + scaleCoord i unit | i <- [1 .. n-1]]
  where
    C dy dx = b - a 
    n = gcd dx dy
    unit = C (dy `quot` n) (dx `quot` n)

-- | Compute a representation of the the angle from the origin to the given
-- coordinate such that the angles are ordered clockwise starting at directly
-- north.
--
-- >>> import Advent.Coord
-- >>> import Data.List (sort)
-- >>> let clockwise = [origin, north, north+north+east, north+east, north+east+east, east, south+east+east, south+east, south+south+east, south, south+south+west, south+west, south+west+west, west, north+west+west, north+west, north+north+west]
-- >>> let angles = map toAngle clockwise
-- >>> angles
-- [(-1) % 1,0 % 1,1 % 3,1 % 2,2 % 3,1 % 1,4 % 3,3 % 2,5 % 3,2 % 1,7 % 3,5 % 2,8 % 3,3 % 1,10 % 3,7 % 2,11 % 3]
--
-- Angles have clockwise arrangement:
--
-- >>> sort angles == angles
-- True
--
-- Angles are independent of scale:
--
-- >>> map toAngle clockwise == map (toAngle . (3*)) clockwise
-- True
toAngle :: Coord -> Rational
toAngle (C y x)
  | x == 0, y == 0 = -1          -- center
  | x >= 0, y <  0 = mk 1 x (-y) -- northeast
  | x >  0, y >= 0 = mk 2 y x    -- southeast
  | x <= 0, y >  0 = mk 3 (-x) y -- southwest
  | otherwise      = mk 4 y x    -- northwest
  where
    -- q in [1,2,3,4]; a >= 0; b > 0
    mk q a b = fromIntegral (q*(a+b)-b) % fromIntegral (a+b)

-- * List utilities

-- | Group the elements of a list using a charactizing function.
--
-- >>> groupOn (`mod` 3) [0..10]
-- [[9,6,3,0],[10,7,4,1],[8,5,2]]
groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f xs = Map.elems (Map.fromListWith (++) [(f x, [x]) | x <- xs])
