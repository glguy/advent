{-# Language BangPatterns, RecordWildCards, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/15>

-}
module Main (main) where

import Advent.Format (format)
import Advent.Coord (Coord, above, below, left, right, origin)
import Advent.Search (bfsOn)
import Intcode (Effect(..), run, new)
import Data.Tree (Tree(Node, rootLabel, subForest))

-- | >>> :main
-- 242
-- 276
main :: IO ()
main =
  do intcode <- [format|15 %d&,%n|]
     let part1:_ = filter (onOxygen . rootLabel)
                 $ explore (searchTree (run (new intcode)))
     let d1 = distance (rootLabel part1)
     print d1
     let d2 = distance (rootLabel (last (explore part1)))
     print (d2 - d1)

data SearchState = SearchState
  { onOxygen :: !Bool  -- ^ Is the robot currently on the oxygen
  , distance :: !Int   -- ^ Commands issued so far
  , location :: !Coord -- ^ robot's current location
  }

explore :: Tree SearchState -> [Tree SearchState]
explore = bfsOn (location . rootLabel) subForest

searchTree :: Effect -> Tree SearchState
searchTree = search False 0 origin

search :: Bool -> Int -> Coord -> Effect -> Tree SearchState
search !oxy !dist !loc e =
  Node (SearchState oxy dist loc)
       [ search (o == 2) (dist + 1) (move loc) e'
       | (i,move) <- [(1,above),(2,below),(3,left),(4,right)]
       , Output o e' <- [e << i], o > 0 ]

(<<) :: Effect -> Int -> Effect
Input f << i = f i
Output o e << i = Output o (e << i)
