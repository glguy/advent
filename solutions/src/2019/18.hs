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
import Data.Array.Unboxed ( UArray, (!), (//), assocs, elems )
import Data.Char (ord, isLower, isUpper)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List ( foldl' )
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Strict qualified as MapStrict
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 2684
-- 1886
main :: IO ()
main =
  do world1 <- getInputArray 2019 18
     let start = head [k | (k,'@') <- assocs world1]

     -- part 1
     print (allKeys world1 [start])

     -- part 2
     let fixups = [(c,'#') | c <- start : cardinal start]
               ++ [(f (g start),'@') | f <- [above, below], g <- [left , right]]
         world2 = world1 // fixups
         start2 = [k | (k,'@') <- assocs world2]
     print (allKeys world2 start2)

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
     | (pos, char) <- assocs (world :: UArray Coord Char)
     , Just cell   <- [charToCell char]
     ]

startSearch :: UArray Coord Char -> Coord -> Cell -> [(Coord, Cell, Int)]
startSearch world start startCell =
  [ (here, cell, n)
  | (here, Just cell, n) <- bfsOn (\(p,_,_)->p) step (start, Just startCell, 0)
  ]
  where
    step (here, hereCell, n)
      | here /= start && isJust hereCell = []
      | otherwise =
         [ (there, thereCell, n+1)
         | there <- cardinal here
         , let char = world ! there
         , let thereCell = charToCell char
         , char /= '#'
         ]

------------------------------------------------------------------------
-- Multiple robot search to gather all keys
------------------------------------------------------------------------

data AllKeys = AllKeys
  { akKeys      :: !IntSet      -- ^ keys found
  , akLocations :: !(Set Coord) -- ^ robot locations
  }
  deriving (Ord, Eq, Show)

allKeys ::
  UArray Coord Char {- ^ world map               -} ->
  [Coord]           {- ^ robot locations         -} ->
  Int               {- ^ search states and costs -}
allKeys world start =
  select $ astar stepAK $ AllKeys IntSet.empty $ Set.fromList start
  where
    keyN   = countBy isLower (elems world)
    done s = IntSet.size (akKeys (fst s)) == keyN
    select = snd . head . filter done

    paths  = extractGraph world

    stepAK AllKeys{..} =
      [ AStep {
        astepNext = AllKeys (IntSet.insert k akKeys)
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
  IntSet ->
  [(Coord, Int, Int)]
nextKey paths start startCell keys =
  [ (here, k, cost)
    | ((here, Key k), cost) <- astarOn fst step (start,startCell) ]
  where
    step (here, hereCell) =
      [ AStep (loc, cell) cost 0
        | case hereCell of
            Key k -> IntSet.member k keys
            _     -> True
        , (loc, cell, cost) <- paths Map.! here
        , case cell of
            Gate i -> IntSet.member i keys
            _      -> True
        ]

keysSSP ::
  UArray Coord Char ->
  Map Coord [(Coord, x, Int)] ->
  Map (Coord, Coord) Int
keysSSP world direct = Map.filterWithKey scrub (foldl' addGen gen0 ks)
  where
    scrub (c1,c2) _
      | Just Key{} <- charToCell (world ! c1)
      , Just Key{} <- charToCell (world ! c2) = True
      | otherwise = False

    ks = Map.keys direct
    gen0 = Map.fromList [ ((src,dst), cost)
                           | (src,dsts) <- Map.toList direct
                           , (dst,_,cost) <- dsts
                           ]

    mkCost Nothing   (Just ik) (Just kj) = [ik+kj]
    mkCost (Just ij) (Just ik) (Just kj) = [min ij (ik+kj)]
    mkCost (Just ij) _ _ = [ij]
    mkCost Nothing _ _ = []

    addGen acc k = MapStrict.fromList
      [ ((i,j), cost)
        | i <- ks
        , j <- ks
        , cost <- mkCost (Map.lookup (i,j) acc)
                         (Map.lookup (i,k) acc)
                         (Map.lookup (k,j) acc)
        ]
