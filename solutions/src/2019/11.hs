{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 11 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/11>

-}
module Main (main) where

import Advent.Format (format)
import Advent.Coord
import Data.Map (Map)
import Data.Map qualified as Map
import Intcode (Effect(..), run, new)

main :: IO ()
main =
  do inp <- [format|11 %d&,%n|]

     let start  = robot origin north (run (new inp))
         run1   = start Map.empty
         run2   = start (Map.singleton origin 1)
         render = putStrLn . drawPicture . fmap paintChar

     render run1
     print (length run1)
     render run2

-- | Run a painter robot to see what it paints.
robot ::
  Coord         {- ^ robot's location             -} ->
  Coord         {- ^ robot's direction            -} ->
  Effect        {- ^ control program effect       -} ->
  Map Coord Int {- ^ starting painted coordinates -} ->
  Map Coord Int {- ^ final painted coordinates    -}
robot here dir effect paint =
  case effect of

    Halt -> paint

    Input f -> robot here dir effect' paint
      where
        color   = Map.findWithDefault 0 here paint
        effect' = f color

    Output color (Output turn effect') -> robot here' dir' effect' paint'
      where
        paint' = Map.insert here color paint
        dir'   = turnFn turn dir
        here'  = here + dir'

    _ -> error "Bad program"

-- | Compute the turn function given a robot's output.
turnFn :: Int {- ^ robot turn output -} -> Coord -> Coord
turnFn 0 = turnLeft
turnFn 1 = turnRight
turnFn x = error ("Unexpected turn command: " ++ show x)

-- | Character representation of paint number.
paintChar :: Int -> Char
paintChar 0 = '░'
paintChar 1 = '█'
paintChar x = error ("Unexpected paint color: " ++ show x)
