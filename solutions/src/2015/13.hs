module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Edge = Edge String String
  deriving (Eq, Ord)

edge :: String -> String -> Edge
edge a b
  | a < b     = Edge a b
  | otherwise = Edge b a

edgeToList :: Edge -> [String]
edgeToList (Edge a b) = [a,b]

main :: IO ()
main =
  do input <- loadInput

     let people1 = uniques (concatMap edgeToList (Map.keys input))
     print (maximumHappiness input people1)

     -- Adding the extra person as the empty string, it's definitely not in the list
     let people2 = "" : people1
     print (maximumHappiness input people2)

neighbors :: [String] -> [Edge]
neighbors [] = []
neighbors (x:xs) = zipWith edge (x:xs) (xs ++ [x])

maximumHappiness ::
  Map Edge Int {- ^ Happiness effects of each edge  -} ->
  [String]     {- ^ List of all people to be seated -} ->
  Int          {- ^ Maximum happiness effect        -}
maximumHappiness relationships people = maximum (score <$> permutations people)
  where
  score xs = sum [Map.findWithDefault 0 e relationships | e <- neighbors xs]

loadInput :: IO (Map Edge Int)
loadInput = Map.fromListWith (+) . map parseLine . lines <$> readFile "input13.txt"

parseLine :: String -> (Edge, Int)
parseLine str =
  case words (filter (/='.') str) of
    [a,_,"gain",n,_,_,_,_,_,_,b] -> (edge a b,   read n)
    [a,_,"lose",n,_,_,_,_,_,_,b] -> (edge a b, - read n)
    _ -> error ("Bad input line: " ++ str)

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList
