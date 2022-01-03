{-# Language TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/24>

Cellular automaton on a hexagonal grid

-}
module Main (main) where

import Advent (counts, times)
import Advent.Coord (Coord, north, east, south, west)
import Advent.Format (format)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data D = De | Dne | Dse | Dw | Dnw | Dsw
pure []

-- |
-- >>> :main
-- 400
-- 3768
main :: IO ()
main =
  do inp <- [format|24 (@D*%n)*|]
     let board = odds (map walk inp)
     print (Set.size board)
     print (Set.size (times 100 step board))

odds :: Ord a => [a] -> Set a
odds = Map.keysSet . Map.filter odd . counts

step :: Set Coord -> Set Coord
step board
  = Map.keysSet
  $ Map.filterWithKey rule
  $ Map.unionsWith (+)
    [Map.mapKeysMonotonic (c +) neighborhood
    | c <- Set.toList board]
  where
    rule k v = v == 2 || v == 1 && Set.member k board

neighborhood :: Map Coord Int
neighborhood = counts (map translate [Dw,De,Dne,Dse,Dnw,Dsw])

walk :: [D] -> Coord
walk = sum . map translate

translate :: D -> Coord
translate Dw  = west
translate De  = east
translate Dne = north + east
translate Dse = south
translate Dnw = north
translate Dsw = south + west
