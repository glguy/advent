{-# Language LambdaCase, ImportQualifiedPost, TransformListComp #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/10>

-}
module Main (main) where

import Advent (getInputMap)
import Advent.Coord (cardinal, invert, invert', south, turnLeft, Coord)
import Advent.Search (dfsN, dfsOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Parse the input and print out answers to both parts.
--
-- >>> :main
-- 6907
-- 541
main :: IO ()
main =
 do input <- getInputMap 2023 10
    let start = head [k | (k, 'S') <- Map.assocs input]
    let dir0 = south -- XXX: hardcoded
    let route = [(here,dir) | (dir,here) <- dfsOn snd (step input) (dir0, dir0+start)]

    let pipe = Set.fromList (map fst route)
    let containable = Map.keysSet input `Set.difference` pipe
    let candidates = Set.fromList (concatMap (rightof input) route) `Set.difference` pipe
    let contained = Set.fromList (dfsN (openNeighbors containable) (Set.toList candidates))
    
    print (length route `quot` 2)
    print (Set.size contained)

openNeighbors :: Set Coord -> Coord -> [Coord]
openNeighbors input x = [y | y <- cardinal x, Set.member y input]

step :: Map Coord Char -> (Coord, Coord) -> [(Coord, Coord)]
step inp (dir, here) =
  [(dir', here + dir') | let dir' = pipeEffect (inp Map.! here) dir]

rightof :: Map Coord Char -> (Coord, Coord) -> [Coord]
rightof input (here, dir) =
  [ here + d
  | let pipe = pipeEffect (input Map.! here)
  , d <- iterate turnLeft (-dir)
  , then drop 1
  , then takeWhile by dir /= pipe d]

pipeEffect :: Char -> Coord -> Coord
pipeEffect = \case
  'S' -> id
  '-' -> id
  '|' -> id
  '7' -> invert
  'J' -> invert'
  'L' -> invert
  'F' -> invert'
  _   -> error "bad pipe character"
