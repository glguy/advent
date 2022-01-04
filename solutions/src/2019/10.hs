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
import Advent.Coord (coordLines, Coord(..))
import Data.List (sortOn)
import Data.Foldable (Foldable(toList), maximumBy)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Ord (comparing)
import Data.Ratio ((%))

-- | >>> :main
-- 227
-- 604
main :: IO ()
main =
 do inp <- getInputLines 10
    let m = Set.fromList [c | (c,'#') <- coordLines inp]

    let (base, vis) = maximumBy (comparing snd)
                      [ (i, countBy (visible m i) m) | i <- toList m ]
    print vis
    let C y x = part2 base (Set.delete base m) !! 199
    print (x * 100 + y)

part2 :: Coord -> Set Coord -> [Coord]
part2 base m
  | Set.null m = []
  | otherwise  = these ++ part2 base (m Set.\\ Set.fromList these)
  where
    these = filter (visible m base) (sortOn (toAngle . subtract base) (toList m))

visible :: Set Coord -> Coord -> Coord -> Bool
visible _ x y | x == y = False
visible ast (C y x) (C v u) =
  and [ Set.notMember (C (v + stepy * i) (u + stepx * i)) ast | i <- [1 .. steps-1] ]
  where
    dx = x - u
    dy = y - v
    steps = gcd dx dy

    stepx = dx `div` steps
    stepy = dy `div` steps

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
