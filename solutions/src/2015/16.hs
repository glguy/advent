{-# Language QuasiQuotes, BlockArguments, LambdaCase #-}
module Main where

import Advent.Format (format)

main :: IO ()
main =
  do input <- [format|16 (Sue %d: (%s: %d)&(, )%n)*|]
     print [i | (i, props) <- input, matchesClues1 props]
     print [i | (i, props) <- input, matchesClues2 props]

matchesClues1 :: [(String,Int)] -> Bool
matchesClues1 = matcher (const (==))

matchesClues2 :: [(String,Int)] -> Bool
matchesClues2 =
  matcher \case
    "cats"        -> (<)
    "trees"       -> (<)
    "pomeranians" -> (>)
    "goldfish"    -> (>)
    _             -> (==)

matcher :: (String -> Int -> Int -> Bool) -> [(String,Int)] -> Bool
matcher match = all \(prop, memory) ->
  match prop (clues prop) memory

clues :: String -> Int
clues "children"    = 3
clues "cats"        = 7
clues "samoyeds"    = 2
clues "pomeranians" = 3
clues "akitas"      = 0
clues "vizslas"     = 0
clues "goldfish"    = 5
clues "trees"       = 3
clues "cars"        = 2
clues "perfumes"    = 1
