module Main where

import Advent (getInputLines)
import Data.List (findIndex, scanl')

main :: IO ()
main =
 do [inp] <- getInputLines 1
    let xs = map interpret inp
    print (sum xs)
    print (part2 xs)

interpret :: Char -> Int
interpret '(' = 1
interpret ')' = -1

part2 :: [Int] -> Maybe Int
part2 = findIndex (< 0) . partialSums

partialSums :: Num a => [a] -> [a]
partialSums = scanl' (+) 0
