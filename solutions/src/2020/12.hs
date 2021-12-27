{-# Language LambdaCase, QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/12>

-}
module Main (main) where

import Advent.Coord
import Advent.Format (format)
import Data.List (foldl')

type Command = Either (D, Int) (T, A)
data D = DN | DS | DE | DW | DF
data T = TL | TR
data A = A90 | A180 | A270
pure[]

-- | The simulation tracks the current location and the vector used
-- when moving /forward/.
data Sim = Sim { here, vect :: !Coord }

-- | Apply an update function to an @a@ typed subcomponent of a
-- @s@ typed value.
type Update s a = (a -> a) -> (s -> s)

mapHere, mapVect :: Update Sim Coord
mapHere f s = s { here = f (here s) }
mapVect f s = s { vect = f (vect s) }

-- |
-- >>> :main
-- 1007
-- 41212
main :: IO ()
main =
  do inp <- [format|12 ((@D%u|@T@A)%n)*|]
     print (walk mapHere (Sim origin east                ) inp)
     print (walk mapVect (Sim origin (move 10 east north)) inp)

walk :: Update Sim Coord -> Sim -> [Command] -> Int
walk f st xs = manhattan origin (here (foldl' (action f) st xs))

action ::
  Update Sim Coord {- ^ cardinal direction component -} ->
  Sim -> Command -> Sim
action mapCard st = \case
  Left  (DN,   n) -> mapCard (move n north    ) st
  Left  (DS,   n) -> mapCard (move n south    ) st
  Left  (DE,   n) -> mapCard (move n east     ) st
  Left  (DW,   n) -> mapCard (move n west     ) st
  Left  (DF,   n) -> mapHere (move n (vect st)) st
  Right (TL, A90) -> mapVect turnLeft           st
  Right (TL,A270) -> mapVect turnRight          st
  Right (TL,A180) -> mapVect turnAround         st
  Right (TR, A90) -> mapVect turnRight          st
  Right (TR,A180) -> mapVect turnAround         st
  Right (TR,A270) -> mapVect turnLeft           st

move :: Int -> Coord -> Coord -> Coord
move n v = addCoord (scaleCoord n v)
