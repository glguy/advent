{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/10>

-}
module Main (main) where

import Advent
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Advent.Search (bfsN, bfsOnN)
import Advent.Coord (Coord, cardinal, north, south, east, west, above, below, right, left)

-- | Parse the input and print out answers to both parts.
--
-- >>> :main
-- 6907
-- 541
main :: IO ()
main =
 do input <- getInputMap 2023 10
    let start = head [k | (k,'S') <- Map.assocs input]
    let coords = [(here,dir) | d <- [south], (_,dir,here) <- bfsOnN pick (step input) [(1,d,d+start)]]

    let pipe = Set.fromList (map fst coords)
    let candidates = Set.fromList (concatMap (rightof input) coords) `Set.difference` pipe
    let contained = Set.fromList (bfsN (openNeighbors input pipe) (Set.toList candidates))
    print (length coords `quot` 2)
    print (Set.size contained)

pick :: (a, b, c) -> c
pick (_,_,here)=here

openNeighbors :: Map Coord a -> Set.Set Coord -> Coord -> [Coord]
openNeighbors input pipe x = [ y  | y <- cardinal x, Map.member y input , Set.notMember y pipe]

step :: Map Coord Char -> (Int, Coord, Coord) -> [(Int, Coord, Coord)]
step inp (n, dir, here) =
 do dir' <- next dir (Map.findWithDefault '.' here inp)
    let here1 = here + dir'
    pure (n+1, dir', here1)

rightof :: Map Coord Char -> (Coord, Coord) -> [Coord]
rightof input (x,dir) =
  case input Map.! x of
    '-' | dir == east -> [below x]
    '-' | dir == west -> [above x]
    '|' | dir == north -> [right x]
    '|' | dir == south -> [left x]
    'F' | dir == west -> [above x, left x]
    'J' | dir == east -> [below x, right x]
    '7' | dir == north -> [right x, above x]
    'L' | dir == south -> [left x, below x]
    _ -> []

next :: Coord -> Char -> [Coord]
next dir '-' | dir == east = [east]
next dir '-' | dir == west = [west]
next dir '|' | dir == north = [north]
next dir '|' | dir == south = [south]
next dir 'J' | dir == south = [west]
next dir 'J' | dir == east = [north]
next dir '7' | dir == east = [south]
next dir '7' | dir == north = [west]
next dir 'F' | dir == west = [south]
next dir 'F' | dir == north = [east]
next dir 'L' | dir == south = [east]
next dir 'L' | dir == west = [north]
next _ _ = []
