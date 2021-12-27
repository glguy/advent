{-# Language ImportQualifiedPost #-}
module Main where

import Advent (chunks)
import Data.List (transpose, scanl')
import Data.Set qualified as Set

data Dir = U | D | L | R   deriving (Read,Show,Ord,Eq)

data Loc = Loc !Int !Int   deriving (Read,Show,Ord,Eq)

origin :: Loc
origin = Loc 0 0

main :: IO ()
main =
  do directions <- loadInput
     print (countHouses 1 directions)
     print (countHouses 2 directions)

countHouses :: Int {- ^ workers -} -> [Dir] -> Int
countHouses n
  = cardinality . concatMap (scanl' step origin) . transpose . chunks n

cardinality :: Ord a => [a] -> Int
cardinality = Set.size . Set.fromList

step :: Loc -> Dir -> Loc
step (Loc x y) dir =
  case dir of
    U -> Loc x (y+1)
    D -> Loc x (y-1)
    L -> Loc (x-1) y
    R -> Loc (x+1) y

loadInput :: IO [Dir]
loadInput = map parseChar <$> readFile "input3.txt"

parseChar :: Char -> Dir
parseChar c =
  case c of
    '^' -> U
    'v' -> D
    '<' -> L
    '>' -> R
    _   -> error ("Bad input character: " ++ [c])
