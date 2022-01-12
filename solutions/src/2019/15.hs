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
import Advent.Coord (Coord, north, south, east, west, origin, drawCoords)
import Advent.Search (bfsOn)
import Intcode (Effect(..), run, new)
import Data.Tree (Tree(Node, rootLabel, subForest))

-- | >>> :main
-- █████████████ ███████████ ███████ █████
-- █     █     █ █         █ █   █   █   █
-- █████ █ ███ █ █ ███ ███ █ ███ █████ ███
--     █ █ █ █   █ █ █   █ █   █       █
-- █ ███ ███ ███ ███ █████ ███ ███████ ███
-- █ █         █   █       █ █           █
-- █ █ ███████ ███ █ █████ █ █████████ ███
-- █ █     █ █ █   █ █   █     █     █ █ █
-- █ ███████ █ █████ ███ █████ █████ █ █ █
-- █         █         █     █     █ █ █
-- █████████ █████ █████ █ ███ ███ █ █ ███
-- █       █   █   █     █ █   █ █   █ █ █
-- █ █████████ █ ███ █████ █ ███ █████ █ █
-- █ █   █     █ █   █   █ █ █           █
-- █ ███ █ █████ █ █████ █ █ ███ ███ █████
--     █ █ █     █ █   █   █   █ █ █ █ █
-- █████ █ █ █ ███ ███ ███ █████ █ █ █ ███
-- █     █ █ █ █   █     █       █ █     █
-- █ █ ███ ███ █ ███ ███ █████ ███ ███████
-- █ █ █     █ █   █ █ █ █     █ █ █     █
-- █ ███████ █ ███ █ █ █ █████ █ █ █ ███ █
-- █         █   █ █ █       █ █ █   █ █ █
-- ███ █████████ █ █ █████ ███ █ █████ █ █
-- █ █ █         █ █     █ █   █       █ █
-- █ █████ ███ ███ █ ███ █ ███████ █████ █
-- █       █ █ █   █   █ █ █       █     █
-- ███ █████ ███ ███████ █ ███ ███ ███ █ █
--   █ █         █     █ █ █   █ █ █ █ █ █
-- █ █ █ █████ █ █ ███ █ █ █ ███ █ █ █ ███
-- █ █ █ █   █ █ █ █ █   █   █   █ █ █ █
-- ███ ███ █ █ ███ █ █████ ███ █ █ █ █ ███
-- █       █ █   █ █       █   █ █   █
-- ███ █████ ███ █ █ ███████ ███ ███ █ ███
--   █ █   █   █   █ █   █   █ █   █ █ █ █
-- █ █ █ ███ █ █████ █ ███ ███ █ ███ ███ █
-- █ █ █ █   █         █     █   █       █
-- █ █ █ █ █████████ ███ █████ ███ ███ ███
-- █ █ █ █     █   █ █   █     █   █ █ █ █
-- █████ ███████ █████ █████████████ ███ █
-- 242
-- 276
main :: IO ()
main =
 do intcode <- [format|2019 15 %d&,%n|]
    let part1:_ = filter (onOxygen . rootLabel)
                    (explore (searchTree (run (new intcode))))
        d1   = distance (rootLabel part1)
        maze = map rootLabel (explore part1)
        d2   = distance (last maze)
    putStr (drawCoords (map location maze))
    print d1
    print (d2 - d1)

data SearchState = SearchState
  { onOxygen :: !Bool  -- ^ Is the robot currently on the oxygen?
  , distance :: !Int   -- ^ Commands issued so far
  , location :: !Coord -- ^ robot's current location
  }

-- | Breadth-first search of the program execution space.
explore :: Tree SearchState -> [Tree SearchState]
explore = bfsOn (location . rootLabel) subForest

-- | Produce the tree of all possible executions of the program.
-- Each tree child will be the result of one of the possible
-- movements at that point in the program.
searchTree :: Effect -> Tree SearchState
searchTree = search (SearchState False 0 origin)

-- | Worker loop for 'searchTree'
search :: SearchState -> Effect -> Tree SearchState
search s@(SearchState oxy dist loc) (Input f) =
  Node s
    [ search (SearchState (o == 2) (dist + 1) (loc + dir)) e'
    | (i, dir) <- [(1,north),(2,south),(3,west),(4,east)]
    , let Output o e' = f i
    , o > 0]
