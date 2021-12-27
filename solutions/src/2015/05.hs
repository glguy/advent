module Main where

import Data.List

main :: IO ()
main =
  do strs <- loadInput
     print (length (filter part1 strs))
     print (length (filter part2 strs))

part1 :: String -> Bool
part1 str = threeVowels str && hasDouble str && noProhibited str

part2 :: String -> Bool
part2 str = pairTwice str && nearby str

threeVowels :: String -> Bool
threeVowels = not . null . drop 2 . filter (`elem` "aeiou")

hasDouble :: String -> Bool
hasDouble = search $ \str ->
                case str of
                  x:y:_ -> x == y
                  _     -> False

noProhibited :: String -> Bool
noProhibited str = not (any (`isInfixOf` str) ["ab","cd","pq","xy"])

search :: (String -> Bool) -> String -> Bool
search p = any p . tails

pairTwice :: String -> Bool
pairTwice = search $ \str ->
                case str of
                  x:y:z -> [x,y] `isInfixOf` z
                  _     -> False

nearby :: String -> Bool
nearby = search $ \str ->
                case str of
                  w:_:y:_ -> w == y
                  _       -> False

loadInput :: IO [String]
loadInput = lines <$> readFile "input5.txt"
