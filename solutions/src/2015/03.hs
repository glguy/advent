{-# Language ImportQualifiedPost, QuasiQuotes #-}
module Main where

import Advent (chunks, format, counts)
import Advent.Coord (Coord, origin, north, east, south, west)
import Data.List (transpose, scanl')

-- | >>> :main
-- 2572
-- 2631
main :: IO ()
main =
 do input <- [format|3 (^|v|<|>)*!|]
    let directions = map parseChar input
    print (countHouses 1 directions)
    print (countHouses 2 directions)

countHouses :: Int {- ^ workers -} -> [Coord] -> Int
countHouses n =
  length . counts . concatMap (scanl' (+) origin) . transpose . chunks n

parseChar :: Char -> Coord
parseChar c =
  case c of
    '^' -> north
    'v' -> south
    '<' -> west
    '>' -> east
    _   -> error ("Bad input character: " ++ [c])
