module Main where

import Advent (count, getInputLines)
import Data.List (tails)

rule :: Char -> Char -> Char
rule x y
  | x == y    = '.'
  | otherwise = '^'

next :: String -> String
next xs = [ rule x y | x:_:y:_ <- tails ("." ++ xs ++ ".") ]

problem :: String -> Int -> Int
problem input n =
  count '.' $ concat $ take n $ iterate next input

main =
  do input <- head <$> getInputLines 18
     print (problem input     40)
     print (problem input 400000)
