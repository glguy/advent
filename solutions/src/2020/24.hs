{-# Language TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/24>

-}
module Main (main) where

import Advent (counts, times)
import Advent.Coord (Coord, addCoord, above, below, left, right, origin)
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
    [Map.mapKeysMonotonic (addCoord c) neighborhood
    | c <- Set.toList board]
  where
    rule k v = v == 2 || v == 1 && Set.member k board

neighborhood :: Map Coord Int
neighborhood = counts [move d origin | d <- [Dw,De,Dne,Dse,Dnw,Dsw]]

walk :: [D] -> Coord
walk = foldl' (flip move) origin

move :: D -> Coord -> Coord
move Dw  = left
move De  = right
move Dne = above . right
move Dse = below
move Dnw = above
move Dsw = below . left
