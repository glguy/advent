{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/16>

-}
module Main where

import Advent (getInputArray)
import Advent.Coord (Coord, east, turnRight, turnLeft)
import Data.Array.Unboxed (UArray, assocs, (!))
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main =
 do input <- getInputArray 2024 16
    let start:_ = [p | (p,'S') <- assocs input]
    let end  :_ = [p | (p,'E') <- assocs input]
    let (p1, p2) = search input Set.empty (IntMap.singleton 0 (Map.singleton (start, east) (Set.singleton start)))
    print p1
    print p2

search ::
  UArray Coord Char ->
  Set (Coord, Coord) ->
  IntMap (Map (Coord, Coord) (Set Coord)) ->
  (Int, Int)
search input seen q =
  case IntMap.minViewWithKey q of
    Nothing -> undefined
    Just ((cost, states), q1)
      | not (null dones) -> (cost, length (mconcat dones))
      | otherwise -> search input seen' q2
      where
        dones = [b | ((e,_),b) <- Map.assocs states1, input!e == 'E']
        seen' = Set.union seen
              $ Set.fromList (Map.keys states1)
        states1 = Map.withoutKeys states seen
        q2 = IntMap.unionWith merge q1
           $ IntMap.fromListWith merge
              [ next
                | ((p,v), path) <- Map.assocs states1
                , next <- [(cost+1000, Map.singleton (p, turnRight v) path)]
                       ++ [(cost+1000, Map.singleton (p, turnLeft v) path)]
                       ++ [(cost+1, Map.singleton (p+v, v) (Set.insert (p+v) path))
                          | let c = input ! (p+v)
                          , c == '.' || c == 'E']
              ]
        merge = Map.unionWith Set.union
           
  