module Main where

import Data.List

main :: IO ()
main =
  do inp <- loadInput
     print (part1 inp)
     print (part2 inp)

loadInput :: IO [Int]
loadInput = map interpret <$> readFile "input1.txt"

interpret :: Char -> Int
interpret '(' = 1
interpret ')' = -1
interpret _   = 0

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Maybe Int
part2 = findIndex (< 0) . partialSums

partialSums :: Num a => [a] -> [a]
partialSums = scanl' (+) 0
