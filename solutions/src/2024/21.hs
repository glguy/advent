{-# Language BlockArguments, ImportQualifiedPost #-}
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
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 177814
-- 220493992841852
main :: IO ()
main =
 do codes <- getInputLines 2024 21
    let score n x = (read (init x) * shortestDoorCodeLength n x)
    print (sum (map (score  2) codes))
    print (sum (map (score 25) codes))

data Pad = Pad (Set Coord) (Map Char Coord)

-- | Turn a list of lines into a 'Pad'. Spaces are removed.
padFromList :: [String] -> Pad
padFromList strs = Pad (Set.fromList (Map.elems buttons)) buttons
  where
    buttons = Map.fromList [(c, p) | (p, c) <- coordLines strs, c /= ' ']

-- | Find the coordinate of a button.
padCoord :: Pad -> Char -> Coord
padCoord (Pad _ m) c = m Map.! c

-- | Test if a coordinate is contained within the pad.
inPad :: Pad -> Coord -> Bool
inPad (Pad s _) x = Set.member x s

-- | The 4-direction pad used to control a robot
robotPad :: Pad
robotPad = padFromList [" ^A", "<v>"]

-- | The 10-digit pad used to control the door
doorPad :: Pad
doorPad = padFromList ["789","456","123"," 0A"]

-- | The length of the shortest input sequence that enters the given
-- door code via a given number of robot layers.
shortestDoorCodeLength ::
  Int    {- ^ robot layers                -} ->
  String {- ^ door code                   -} ->
  Int    {- ^ shortest button press count -}
shortestDoorCodeLength n str =
  sum
    [ minimum (map (shortestRobotCodeLength n) keys)
    | keys <- route doorPad str
    ]

-- | The length of the shortest input sequence that enters the given
-- robot directional code via a given number of robot layers.
shortestRobotCodeLength ::
  Int    {- ^ robot layers                -} ->
  String {- ^ door code                   -} ->
  Int    {- ^ shortest button press count -}
shortestRobotCodeLength = memo2 \n str ->
  if n == 0 then length str else
  sum
    [ minimum (map (shortestRobotCodeLength (n - 1)) keys)
    | keys <- route robotPad str
    ]

-- | Find a list of steps needed to input a code on a pad. The inner
-- lists allow for there to be multiple, valid subsequences that exist
-- for some keys. Only the most direct routes are considered. This
-- takes advantage of our input pads only missing corners. Sequences
-- always start from @A@.
--
-- >>> route doorPad "029A"
-- [["<A"],["^A"],["^^>A",">^^A"],["vvvA"]]
route :: Pad -> String -> [[String]]
route pad str = zipWith (walk pad) (padCoord pad 'A' : absolutes) absolutes
  where
    absolutes = map (padCoord pad) str

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
  , keys <- [        rawKeys | inPad pad (C y2 x1) ]
         ++ [reverse rawKeys | y1 /= y2, x1 /= x2, inPad pad (C y1 x2)]
  ]
