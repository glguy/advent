{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/23>

Search for the cheapest way to rearrange lizards in a maze
to get all the lizards back into the correct rooms.

-}
module Main (main) where

import Advent.Coord (Coord(..), coordCol, below, manhattan, cardinal)
import Advent.Input (getInputMap)
import Advent.Search (AStep(..), dfs, astar)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Char (ord, isLetter)

data Cell = Open | Amphi { amphiTarget :: !Int, amphiCost :: !Int }
  deriving (Eq, Ord, Show)

-- | Compute all the information needed from a character in the input map.
toCell :: Char -> Maybe Cell
toCell '.' = Just Open
toCell a | isLetter a = Just $! Amphi (3 + 2 * (ord a - 65)) (10 ^ (ord a - 65))
toCell _ = Nothing

-- | Predicate for rooms (rather than hallways)
isRoom :: Coord -> Bool
isRoom (C _ c) = c == 3 || c == 5 || c == 7 || c == 9

main :: IO ()
main =
 do inp <- Map.mapMaybe toCell <$> getInputMap 23
    print (head [cost | (w, cost) <- astar step inp, done w])

-- | Step the simulation once tracking the cost of the move.
step :: Map Coord Cell -> [AStep (Map Coord Cell)]
step w =
  [ AStep { astepNext = Map.insert c Open (Map.insert dest a w)
          , astepCost = manhattan c dest * stepCost
          , astepHeuristic = 0 }
  | (c, a@(Amphi target stepCost)) <- Map.toList w
  , dest <- route w c
  , if isRoom c
      then not (isRoom dest)
        && not (roomClean w (coordCol c))
      else isRoom dest
        && coordCol dest == target
        && roomClean w target
        && maybe True (a==) (Map.lookup (below dest) w)
  ]

-- | Check that all the amphis in a room are supposed to be there.
roomClean :: Map Coord Cell -> Int -> Bool
roomClean w c = go 2
  where
    go r =
      case Map.lookup (C r c) w of
        Just Open -> go (r+1)
        Just (Amphi t _) -> t == c && go (r+1)
        Nothing -> True

-- | Check that everything on the map is where it should be.
done :: Map Coord Cell -> Bool
done w =
  all (\(k,v) ->
      case v of
        Open -> True
        Amphi t _ -> coordCol k == t)
  (Map.toList w)

route :: Map Coord Cell -> Coord -> [Coord]
route w = dfs move
  where
    move c = [c' | c' <- cardinal c, Map.lookup c' w == Just Open]
