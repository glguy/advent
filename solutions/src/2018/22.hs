{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/22>

This solution uses an A* graph search to find the shortest path through
the cave to the target.

I picked an arbitrary cave size to memoize that was big enough to avoid
indexing errors. I use a boxed array so that I can lazily compute the
geologic indexes of the various locations in the cave. This also allows
me to recursively define 'geologic' and 'erosion'.

My A* heuristic is manhattan distance to the target plus a penalty for
not holding the torch. (Accounting for the torch saves a small, but
positive amount of time.)

Instead of modelling the tool being held directly I simply keep track of
the risk number of the area I'm not allowed to enter.

-}
module Main (main) where

import Advent (format)
import Advent.Coord (Coord(C), above, left, cardinal, manhattan, origin)
import Advent.Memo (memo)
import Advent.Search (astar, AStep(AStep))
import Data.Array qualified as A
import Data.List (delete)

-- | Print the answers to day 22
main :: IO ()
main =
  do (depth, tx, ty) <- [format|22 depth: %u%ntarget: %u,%u%n|]
     let target = C ty tx
     let risk = mkRisk depth target
     print (part1 risk target)
     print (part2 risk target)

-- | Sum of risk values in rectangle defined by origin and target
part1 :: (Coord -> Tool) -> Coord -> Int
part1 risk target = sum [toolId (risk c) | c <- A.range (origin, target)]

-- | Minimum cost of traveling to the target from the origin
part2 :: (Coord -> Tool) -> Coord -> Int
part2 risk target = n
  where
    Just n = lookup goal (astar (steps risk target) start)
    start  = Node origin torch
    goal   = Node target torch

-- tool representation -------------------------------------------------

-- | Tools track the risk index that they are incompatible with.
newtype Tool = Tool { toolId :: Int } deriving (Show, Eq, Ord)

-- | The torch tool is used at the beginning and end of the trip.
torch :: Tool
torch = Tool 1 -- torch is excluded from wet (1) squares

-- | List of all three tools.
tools :: [Tool]
tools = [Tool 0, Tool 1, Tool 2]

-- movement rules ------------------------------------------------------

-- | Graph search node. There will be a lot of these and this
-- representation is much more compact than a tuple. This will represent
-- a 3D location in the graph search: current position and current tool.
data Node = Node {-# Unpack #-}!Coord {-# Unpack #-}!Tool deriving (Eq, Ord)

-- | Compute the states reachable from the given state. Cost is the
-- incremental cost of choosing that state. Heuristic is lower-bound on
-- the distance remaining until the target. This lower-bound is an
-- admissible heuristic that enables A* to find the optimal path.
steps ::
  (Coord -> Tool) {- ^ location to banned tool         -} ->
  Coord           {- ^ target location                 -} ->
  Node            {- ^ location, tool                  -} ->
  [AStep Node]    {- ^ location, tool, cost, heuristic -}
steps risk target (Node here tool) =
  [ AStep (Node dest tool') cost heuristic
     | (Node dest tool', cost) <- changeTool ++ move
     , risk dest /= tool'
     , let heuristic = manhattan dest target
                     + if tool' == torch then 0 else 7
     ]
  where
    changeTool = [(Node here tool', 7) | tool' <- delete tool tools ]
    move = [(Node dst tool, 1) | dst@(C y x) <- cardinal here, y >= 0, x >= 0]

-- cave characterization -----------------------------------------------

-- | Computes a function that can query the risk index at a given query
-- coordinate. The query function is backed by an array to efficiently
-- compute risks for a given depth and target value.
mkRisk ::
  Int   {- ^ layer depth                    -} ->
  Coord {- ^ target coordinate              -} ->
  Coord {- ^ query coordinate               -} ->
  Tool  {- ^ risk index at query coordinate -}
mkRisk depth target = \i -> Tool (erosion i `rem` 3)
  where
    geologic c@(C y x)
      | y == 0      = x * 16807
      | x == 0      = y * 48271
      | c == target = 0
      | otherwise   = erosion (above c) * erosion (left c)

    erosion = memo (\i -> (geologic i + depth) `rem` 20183)
