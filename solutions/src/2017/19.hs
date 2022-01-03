{-|
Module      : Advent.Coord
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/19>

Day 19 has us follow an ASCII art trail and report on the letters
we find along the way as well as the total trail path length.

-}
module Main where

import Advent.Input (getInputArray)
import Advent.Coord (Coord(..), north, east, south, west)
import Data.Char (isAlpha)
import Data.Array.Unboxed (UArray, (!), assocs)

-- | Print the solutions to both parts of day 19. Input file can be
-- overridden via the command-line arguments.
main :: IO ()
main =
 do input <- getInputArray 19

    let start:_ = [c | (c@(C 0 _), '|') <- assocs input]
        path = toPath input south start

    putStrLn (filter isAlpha path)
    print (length path)

-- | Return the path given a map, current travel direction,
-- and current location.
toPath ::
  UArray Coord Char {- ^ map       -} ->
  Coord             {- ^ direction -} ->
  Coord             {- ^ location  -} ->
  String            {- ^ path      -}
toPath grid d c =
  let isPath d' = grid ! (c + d') /= ' '
      next d'   = toPath grid d' (c + d') in
  case grid ! c of
    ' '                            -> []
    '+' | d /= south, isPath north -> '+' : next north
        | d /= north, isPath south -> '+' : next south
        | d /= west,  isPath east  -> '+' : next east
        | d /= east,  isPath west  -> '+' : next west
    a                              -> a   : next d
