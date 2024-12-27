{-# Language BlockArguments, LambdaCase, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/21>

>>> :{
:main + "029A
980A
179A
456A
379A
"
:}
126384
154115708116294

-}
module Main (main) where

import Advent (getInputLines)
import Advent.Coord (Coord(..), coordLines)
import Advent.Memo (memo2)
import Data.Map (Map, (!))
import Data.Map qualified as Map

-- | >>> :main
-- 177814
-- 220493992841852
main :: IO ()
main =
 do codes <- getInputLines 2024 21
    let score n x = (read (init x) * shortDoorCode n x)
    print (sum (map (score  2) codes))
    print (sum (map (score 25) codes))

type Pad = Map Char Coord

-- | Turn a list of lines into a 'Pad'. Spaces are removed.
padFromList :: [String] -> Pad
padFromList strs = Map.fromList [(c, p) | (p, c) <- coordLines strs]

-- | The 4-direction pad used to control a robot
robotPad :: Pad
robotPad = padFromList [" ^A", "<v>"]

-- | The 10-digit pad used to control the door
doorPad :: Pad
doorPad = padFromList ["789","456","123"," 0A"]

-- | The length of the shortest input sequence that enters the given
-- door code via a given number of robot layers.
shortDoorCode ::
  Int    {- ^ robot layers                -} ->
  String {- ^ door code                   -} ->
  Int    {- ^ shortest button press count -}
shortDoorCode n =
  sum . map (minimum . map (shortRobotCode n)) . route doorPad

-- | The length of the shortest input sequence that enters the given
-- robot directional code via a given number of robot layers.
shortRobotCode ::
  Int    {- ^ robot layers                -} ->
  String {- ^ robot arrows code           -} ->
  Int    {- ^ shortest button press count -}
shortRobotCode = memo2 \case
  0 -> length
  n -> sum . map (minimum . map (shortRobotCode (n - 1))) . route robotPad

-- | Find a list of steps needed to input a code on a pad. The inner
-- lists allow for there to be multiple, valid subsequences that exist
-- for some keys. Only the most direct routes are considered. This
-- takes advantage of our input pads only missing corners. Sequences
-- always start from @A@.
--
-- >>> route doorPad "029A"
-- [["<A"],["^A"],["^^>A",">^^A"],["vvvA"]]
route :: Pad -> String -> [[String]]
route pad str = zipWith (walk pad) (pad ! 'A' : absolutes) absolutes
  where
    absolutes = map (pad !) str

-- | Find the unique, shortest paths to move from one location to
-- another on a pad.
walk :: Pad -> Coord -> Coord -> [String]
walk pad (C y1 x1) (C y2 x2) =
  [ keys ++ "A"
  | let rawKeys =
          replicate (y1 - y2) '^' ++
          replicate (y2 - y1) 'v' ++
          replicate (x2 - x1) '>' ++
          replicate (x1 - x2) '<'
  , keys <- [        rawKeys |                     pad ! ' ' /= C y2 x1]
         ++ [reverse rawKeys | y1 /= y2, x1 /= x2, pad ! ' ' /= C y1 x2]
  ]
