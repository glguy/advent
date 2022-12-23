{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/23>

>>> :{
:main +
  ".......#......\n\
  \.....###.#....\n\
  \...#...#.#....\n\
  \....#...##....\n\
  \...#.###......\n\
  \...##.#.##....\n\
  \....#..#......\n"
:}
110
20

-}
module Main where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Ix (range)
import Data.List (tails)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Advent (getInputMap, countBy, counts)
import Advent.Coord (above, below, boundingBox, left, neighbors, right, Coord)

-- |
-- >>> :main
-- 4236
-- 1023
main :: IO ()
main =
 do input <- Map.keysSet . Map.filter ('#'==) <$> getInputMap 2022 23
    let states = sim input
    
    -- part 1
    let b = states !! 10
    print case boundingBox (Set.toList b) of
      Just box -> countBy (`Set.notMember` b) (range box)
      Nothing -> 0

    -- part 2
    print (sameIx 1 states)

sameIx :: Int -> [Set Coord] -> Int
sameIx i (x:y:z)
  | x == y = i
  | otherwise = sameIx (i+1) (y:z)
sameIx _ _ = undefined

sim :: Set Coord -> [Set Coord]
sim start = scanl (\elves move -> step (move elves) elves) start moves

step :: (Coord -> Maybe Coord) -> Set Coord -> Set Coord
step m board =
   Set.union (Map.keysSet ok)
     (Set.difference board (Map.keysSet steps'))
   where
      steps = Map.fromList [(e, t) | e <- Set.toList board, any (`Set.member` board) (neighbors e), Just t <- [m e]]
      ok = Map.filter (== 1) (counts steps)
      steps' = Map.filter (\t -> Map.member t ok) steps

nMove :: Set Coord -> Coord -> Maybe Coord
nMove board here  =
 do guard (all (\x -> Set.notMember x board)
          [above here, above (right here), above (left here)])
    Just (above here)

sMove :: Set Coord -> Coord -> Maybe Coord
sMove board here  =
 do guard (all (\x -> Set.notMember x board)
          [below here, below (right here), below (left here)])
    Just (below here)

eMove :: Set Coord -> Coord -> Maybe Coord
eMove board here  =
 do guard (all (\x -> Set.notMember x board)
          [right here, right (above here), right (below here)])
    Just (right here)

wMove :: Set Coord -> Coord -> Maybe Coord
wMove board here  =
 do guard (all (\x -> Set.notMember x board)
          [left here, left (above here), left (below here)])
    Just (left here)

moves :: [Set Coord -> Coord -> Maybe Coord]
moves = map (combine . take 4) (tails (cycle [nMove, sMove, wMove, eMove]))
  where
   combine [] _ _ = Nothing
   combine (x:xs) board here = x board here <|> combine xs board here
