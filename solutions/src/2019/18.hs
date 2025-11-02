{-# Language RecordWildCards, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/18>

Approach:

1. Reduce maze to a graph with 'extractGraph'
   Nodes: starting points, gates, keys
   Edges: shortest direct route between nodes

2. Implement 'nextKey' function to find list of reachable keys
   for a particular robot.

3. Use Djikstra search to search the space of picking a robot to move
   from its current position to an unvisited key until
   all keys are visited.

-}
module Main (main) where

import Advent (getInputArray, countBy)
import Advent.Coord (above, below, cardinal, left, right, Coord)
import Advent.Search (astar, astarOn, bfsOn, AStep(..))
import Advent.SmallSet (SmallSet)
import Advent.SmallSet qualified as SmallSet
import Data.Array.Unboxed ( UArray, (!), (//), assocs, elems, range )
import Data.Char (ord, isLower, isUpper)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 2684
-- 1886
main :: IO ()
main =
  do world1 <- getInputArray 2019 18

     -- part 1
     print (allKeys world1)

     -- part 2
     let [start] = findStarts world1
         fixups = zip (range (above (left start), below (right start))) "@#@\ \###\ \@#@"
         world2 = world1 // fixups
     print (allKeys world2)

findStarts :: UArray Coord Char -> [Coord]
findStarts world = [k | (k, '@') <- assocs world]

------------------------------------------------------------------------
-- Search that finds shortest distances to the remaining keys
------------------------------------------------------------------------

data Cell = Start | Gate !Int | Key !Int
  deriving (Eq, Show, Ord)

charToCell :: Char -> Maybe Cell
charToCell x
  | '@' == x  = Just Start
  | isLower x = Just (Key  (ord x - ord 'a'))
  | isUpper x = Just (Gate (ord x - ord 'A'))
  | otherwise = Nothing

------------------------------------------------------------------------
-- Simplify down to starts, keys, gates, and paths between them
------------------------------------------------------------------------

extractGraph :: UArray Coord Char -> Map Coord [(Coord, Cell, Int)]
extractGraph world =
  Map.fromList
  [ (pos, startSearch world pos cell)
     | (pos, char) <- assocs world
     , Just cell   <- [charToCell char]
     ]

startSearch :: UArray Coord Char -> Coord -> Cell -> [(Coord, Cell, Int)]
startSearch world start startCell =
  [ (here, cell, n)
  | (here, Just cell, n) <- bfsOn (\(p,_,_)->p) step (start, Just startCell, 0)
  ]
  where
    step (here, hereCell, n) =
      [ (there, charToCell char, n+1)
      | isNothing hereCell || here == start
      , there <- cardinal here
      , let char = world ! there
      , char /= '#'
      ]

------------------------------------------------------------------------
-- Multiple robot search to gather all keys
------------------------------------------------------------------------

data AllKeys = AllKeys
  { akKeys      :: !SmallSet      -- ^ keys found
  , akLocations :: !(Set Coord) -- ^ robot locations
  }
  deriving (Ord, Eq, Show)

allKeys ::
  UArray Coord Char {- ^ world map               -} ->
  Int               {- ^ search states and costs -}
allKeys world = head [cost | (s, cost) <- astar stepAK startState, done s]
  where
    starts = findStarts world
    startState = AllKeys SmallSet.empty (Set.fromList starts)
    keyN = countBy isLower (elems world)
    done s = SmallSet.size (akKeys s) == keyN

    -- cache all the paths between interesting nodes
    paths = extractGraph world

    stepAK AllKeys{..} =
      [ AStep {
        astepNext = AllKeys (SmallSet.insert k akKeys)
                            (Set.insert loc (Set.delete who akLocations)),
        astepCost = cost,
        astepHeuristic = 0 }
        | who <- Set.toList akLocations
        , let Just whoCell = charToCell (world ! who)
        , (loc, k, cost) <- nextKey paths who whoCell akKeys
        ]

------------------------------------------------------------------------
-- Single robot moves to adjacent, unvisited keys
------------------------------------------------------------------------

nextKey ::
  Map Coord [(Coord, Cell, Int)] ->
  Coord ->
  Cell ->
  SmallSet ->
  [(Coord, Int, Int)]
nextKey paths start startCell keys =
  [ (here, k, cost)
    | ((here, Key k), cost) <- astarOn fst step (start, startCell)
    , not (SmallSet.member k keys) ]
  where
    step (here, hereCell) =
      [ AStep (loc, cell) cost 0
        | case hereCell of
            Key k -> SmallSet.member k keys -- stop when we reach a new key
            _     -> True
        , (loc, cell, cost) <- paths Map.! here
        , case cell of
            Gate i -> SmallSet.member i keys
            _      -> True
        ]
