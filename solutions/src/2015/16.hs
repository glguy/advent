module Main where

import Data.Char (isPunctuation)

main :: IO ()
main =
  do input <- loadInput
     print [lookup "Sue" props | props <- input, matchesClues1 props]
     print [lookup "Sue" props | props <- input, matchesClues2 props]

matchesClues1 :: [(String,Int)] -> Bool
matchesClues1 = matcher (const (==))

matchesClues2 :: [(String,Int)] -> Bool
matchesClues2 =
  matcher $ \prop ->
    case prop of
      "cats"        -> (<)
      "trees"       -> (<)
      "pomeranians" -> (>)
      "goldfish"    -> (>)
      _             -> (==)

matcher :: (String -> Int -> Int -> Bool) -> [(String,Int)] -> Bool
matcher match = all $ \(prop,memory) ->
  case lookup prop clues of
    Nothing           -> True
    Just mfcsam       -> match prop mfcsam memory

clues :: [(String,Int)]
clues = parseLine
  " children   : 3 \
  \ cats       : 7 \
  \ samoyeds   : 2 \
  \ pomeranians: 3 \
  \ akitas     : 0 \
  \ vizslas    : 0 \
  \ goldfish   : 5 \
  \ trees      : 3 \
  \ cars       : 2 \
  \ perfumes   : 1 "

loadInput :: IO [[(String,Int)]]
loadInput = map parseLine . lines <$> readFile "input16.txt"

parseLine :: String -> [(String,Int)]
parseLine = asProps . words . filter (not . isPunctuation)

asProps :: [String] -> [(String,Int)]
asProps []      = []
asProps (x:y:z) = (x,read y) : asProps z
asProps [_]     = error "props mismatched"
