{-# Language TransformListComp #-}
module Main where

import Data.Bits (Bits(popCount))
import Advent.Coord (Coord(..), cardinal)
import Advent.Search (bfsOn)

myInput :: Int
myInput = 1350

data Entry = Entry { entrySteps :: !Int, entryCoord :: Coord }
  deriving (Eq, Show)

main :: IO ()
main =
  do let entries = bfsOn entryCoord nextEntries initialEntry
     print [ steps | Entry steps (C 39 31) <- entries, then take 1 ]
     print $ length [ () | Entry steps _ <- entries
                         , then takeWhile by steps <= 50 ]

initialEntry :: Entry
initialEntry = Entry 0 (C 1 1)

isValidCoord :: Coord -> Bool
isValidCoord (C y x) =
  x >= 0 && y >= 0 &&
  even (popCount (x*x + 3*x + 2*x*y + y + y*y + myInput))

nextEntries :: Entry -> [Entry]
nextEntries (Entry steps coord)
  = [ Entry (steps+1) c | c <- cardinal coord, isValidCoord c ]
