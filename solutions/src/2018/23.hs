{-# Language QuasiQuotes #-}
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
import Data.Ord (comparing)
import Data.List (maximumBy)
import Advent.Coord3 (manhattan, origin, Coord3(..))

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

part1 :: [Bot] -> Int
part1 bots = countBy (\(Bot bot _) -> manhattan bot here <= r) bots
  where
    Bot here r = maximumBy (comparing (\(Bot _ r) -> r)) bots

part2 :: [Bot] -> Int
part2 bots = manhattan point origin
  where
    candidates = concatMap octohedron bots
    visibleFrom pos = countBy (\(Bot botPos botRad) -> manhattan pos botPos <= botRad) bots
    out = [(p, visibleFrom p) | p <- candidates]
    a = maximumBy (comparing snd) out
    point = minimize 10000 visibleFrom (fst a)

octohedron :: Bot -> [Coord3]
octohedron (Bot (C3 x y z) r) =
  [ C3 (x+r) y z
  , C3 (x-r) y z
  , C3 x (y+r) z
  , C3 x (y-r) z
  , C3 x y (z+r)
  , C3 x y (z-r)
  ]

minimize :: Int -> (Coord3 -> Int) -> Coord3 -> Coord3
minimize d f p@(C3 x y z) =
  case [ p' | p' <- candidates, f p <= f p' ] of
    p' : _         -> minimize d f p'
    [] | d > 1     -> minimize (d`quot`2) f p
       | otherwise -> p
  where
    candidates =
      [ C3 (x-d) (y-d) (z-d)

      , C3 (x-d) (y-d) z
      , C3 x (y-d) (z-d)
      , C3 (x-d) y (z-d)

      , C3 (x-d) y z
      , C3 x (y-d) z
      , C3 x y (z-d)
      ]
