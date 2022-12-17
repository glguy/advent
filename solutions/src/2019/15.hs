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

-- |
-- >>> :main
-- █████████████·███████████·███████·█████
-- █·····█·····█·█·········█·█···█···█···█
-- █████·█·███·█·█·███·███·█·███·█████·███
-- ····█·█·█·█···█·█·█···█·█···█·······█··
-- █·███·███·███·███·█████·███·███████·███
-- █·█·········█···█·······█·█···········█
-- █·█·███████·███·█·█████·█·█████████·███
-- █·█·····█·█·█···█·█···█·····█·····█·█·█
-- █·███████·█·█████·███·█████·█████·█·█·█
-- █·········█·········█·····█·····█·█·█··
-- █████████·█████·█████·█·███·███·█·█·███
-- █·······█···█···█·····█·█···█·█···█·█·█
-- █·█████████·█·███·█████·█·███·█████·█·█
-- █·█···█·····█·█···█···█·█·█···········█
-- █·███·█·█████·█·█████·█·█·███·███·█████
-- ····█·█·█·····█·█···█···█···█·█·█·█·█··
-- █████·█·█·█·███·███·███·█████·█·█·█·███
-- █·····█·█·█·█···█·····█·······█·█·····█
-- █·█·███·███·█·███·███·█████·███·███████
-- █·█·█·····█·█···█·█·█·█·····█·█·█·····█
-- █·███████·█·███·█·█·█·█████·█·█·█·███·█
-- █·········█···█·█·█·······█·█·█···█·█·█
-- ███·█████████·█·█·█████·███·█·█████·█·█
-- █·█·█·········█·█·····█·█···█·······█·█
-- █·█████·███·███·█·███·█·███████·█████·█
-- █·······█·█·█···█···█·█·█·······█·····█
-- ███·█████·███·███████·█·███·███·███·█·█
-- ··█·█·········█·····█·█·█···█·█·█·█·█·█
-- █·█·█·█████·█·█·███·█·█·█·███·█·█·█·███
-- █·█·█·█···█·█·█·█·█···█···█···█·█·█·█··
-- ███·███·█·█·███·█·█████·███·█·█·█·█·███
-- █·······█·█···█·█·······█···█·█···█····
-- ███·█████·███·█·█·███████·███·███·█·███
-- ··█·█···█···█···█·█···█···█·█···█·█·█·█
-- █·█·█·███·█·█████·█·███·███·█·███·███·█
-- █·█·█·█···█·········█·····█···█·······█
-- █·█·█·█·█████████·███·█████·███·███·███
-- █·█·█·█·····█···█·█···█·····█···█·█·█·█
-- █████·███████·█████·█████████████·███·█
-- 242
-- 276
main :: IO ()
main =
 do intcode <- [format|2019 15 %d&,%n|]
    let part1:_ = filter onOxygen
                    (explore (initialState (run (new intcode))))
        part2 = explore part1{distance = 0}
    putStr (drawCoords (map location part2))
    print (distance part1)
    print (distance (last part2))

data SearchState = SearchState
  { onOxygen :: !Bool  -- ^ Is the robot currently on the oxygen?
  , distance :: !Int   -- ^ Commands issued so far
  , location :: !Coord -- ^ robot's current location
  , effect   :: Effect -- ^ current program state
  }

-- | Build a new search state for a program effect. Assume we're at the
-- origin and that we are not on oxygen.
initialState :: Effect -> SearchState
initialState = SearchState False 0 origin

-- | Breadth-first search of the program execution visiting each
-- location in the maze once.
explore :: SearchState -> [SearchState]
explore = bfsOn location step

-- | Advance the program forward one input/output step.
step :: SearchState -> [SearchState]
step (SearchState oxy dist loc (Input f)) =
  [ SearchState (o == 2) (dist + 1) (loc + dir) e'
    | (i, dir) <- [(1,north),(2,south),(3,west),(4,east)]
    , let Output o e' = f i
    , o > 0]
