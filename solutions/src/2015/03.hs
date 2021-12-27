{-# Language ImportQualifiedPost, QuasiQuotes #-}
module Main where

import Advent (chunks, format, counts)
import Advent.Coord (above, below, left, origin, right, Coord)
import Data.List (transpose, scanl')

main :: IO ()
main =
 do input <- [format|3 (^|v|<|>)*!|]
    let directions = map parseChar input
    print (countHouses 1 directions)
    print (countHouses 2 directions)

countHouses :: Int {- ^ workers -} -> [Coord -> Coord] -> Int
countHouses n =
  length . counts . concatMap (scanl' (\x f -> f x) origin) . transpose . chunks n

parseChar :: Char -> Coord -> Coord
parseChar c =
  case c of
    '^' -> above
    'v' -> below
    '<' -> left
    '>' -> right
    _   -> error ("Bad input character: " ++ [c])
