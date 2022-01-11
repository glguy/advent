{-# Language ImportQualifiedPost, RecordWildCards, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/22>

-}
module Main where

import Advent (format, arrIx)
import Advent.Coord (Coord(..), manhattan, cardinal, boundingBox, origin)
import Advent.Search (AStep(AStep), astar)
import Data.Array (Array)
import Data.Array qualified as Array

data Node = Node { nodeSize, nodeUsed :: !Int }
  deriving (Show)

-- Hardcode a constant I computed for my particular input.
-- This problem doesn't favor a general solution, so I didn't
-- bother encoding my analysis of what constitutes an immobile
-- node into my program

cutoff :: Int
cutoff = 100

main :: IO ()
main =
 do input <- [format|2016 22
      root%@ebhq-gridcenter# df -h%n
      Filesystem              Size  Used  Avail  Use%%%n
      (/dev/grid/node-x%u-y%u *%uT *%uT *%uT *%u%%%n)*
      |]
    let grid = toArray [(C y x, Node sz use) | (x,y,sz,use,_,_) <- input]
    let start = findStart grid
        hole  = findHole grid
    print $ viable grid
    print $ head
            [ cost | (ss, cost) <- astar (next grid) (SearchState start hole)
                  , searchGoal ss == origin
                  ]

viable :: Array Coord Node -> Int
viable grid = length
  [() | (c1,n1) <- Array.assocs grid
      , (c2,n2) <- Array.assocs grid
      , c1 /= c2
      , nodeUsed n1 /= 0
      , nodeUsed n1 <= nodeSize n2 - nodeUsed n2 ]

findStart :: Array Coord e -> Coord
findStart grid =
  maximum [C 0 x | C 0 x <- Array.range (Array.bounds grid)]

findHole :: Array Coord Node -> Coord
findHole grid = head [ c | (c,n) <- Array.assocs grid, nodeUsed n == 0 ]

data SearchState = SearchState
  { searchGoal, searchHole :: !Coord }
  deriving (Eq, Ord, Read, Show)

next :: Array Coord Node -> SearchState -> [AStep SearchState]
next grid SearchState{..} =
  [ AStep (SearchState newGoal newHole) 1 h
     | newHole <- cardinal searchHole
     , node <- arrIx grid newHole
     , nodeSize node < cutoff
     , let newGoal
             | searchGoal == newHole = searchHole
             | otherwise             = searchGoal

           h = manhattan newGoal origin
             + manhattan newHole newGoal
             - 1 ]

toArray :: [(Coord, a)] -> Array Coord a
toArray xs = Array.array bnds xs
  where
    Just bnds = boundingBox (map fst xs)
