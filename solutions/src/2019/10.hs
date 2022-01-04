{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/10>

-}
module Main (main) where

import Advent (getInputLines, countBy)
import Advent.Coord (coordLines, Coord(..), manhattan, scaleCoord)
import Data.List (sortOn, transpose)
import Data.Foldable (Foldable(toList), maximumBy)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Ratio ((%))

-- | >>> :main
-- 227
-- 604
main :: IO ()
main =
 do inp <- getInputLines 10
    let m = Set.fromList [c | (c,'#') <- coordLines inp]

    let (base, vis) = maximumBy (comparing snd) [(i, countBy (lineOfSight m i) m) | i <- toList m]
    print vis
    let C y x = spiral base m !! 200
    print (x * 100 + y)

spiral :: Coord -> Set Coord -> [Coord]
spiral base
  = concat
  . transpose
  . map (sortOn (manhattan base))
  . groupOn (toAngle . subtract base)
  . Set.toList

groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn f xs = Map.elems (Map.fromListWith (++) [(f x, [x]) | x <- xs])

-- | Return all the locations that fall on a line between two coordinates.
between :: Coord -> Coord -> [Coord]
between a b = [a + scaleCoord i unit | i <- [1 .. n-1]]
  where
    C dy dx = b - a 
    n = gcd dx dy
    unit = C (dy `quot` n) (dx `quot` n)

-- | Check if two asteroids have line of sight between each other.
lineOfSight :: Set Coord -> Coord -> Coord -> Bool
lineOfSight ast a b = a /= b && and [Set.notMember c ast | c <- between a b]

-- | Angles ordered starting from pointing up and proceeding
-- clockwise.
data Angle = Angle !Int !Rational -- quadrant and slope
  deriving (Eq, Ord, Show)

-- | Compute the angle from the origin to a given coordinate.
toAngle :: Coord -> Angle
toAngle (C y x)
  | x == 0, y == 0 = Angle 0 0
  | x >= 0, y <  0 = mk 1 x (-y)    -- upper right
  | x >  0, y >= 0 = mk 2 y x       -- lower right
  | x <= 0, y >  0 = mk 3 (-x) y    -- lower left
  | otherwise      = mk 4 (-y) (-x) -- upper left
  where
    mk i a b = Angle i (fromIntegral a % fromIntegral b)
