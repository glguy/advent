{-# Language QuasiQuotes, TemplateHaskell #-}
module Main where

import Advent
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data E = Egain | Elose deriving Show

mempty

data Edge = Edge String String
  deriving (Eq, Ord)

edge :: String -> String -> Edge
edge a b
  | a < b     = Edge a b
  | otherwise = Edge b a

main :: IO ()
main =
 do input <- [format|2015 13 (%s would @E %u happiness units by sitting next to %s.%n)*|]
    let graph = Map.fromListWith (+) [(edge x y, case e of Egain -> v; Elose -> -v) | (x,e, v,y) <- input]
    let people1 = uniques [z | (x,_,_,y) <- input, z <- [x,y]]
    print (maximumHappiness graph people1)

    -- Adding the extra person as the empty string, it's definitely not in the list
    let people2 = "" : people1
    print (maximumHappiness graph people2)

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

uniques :: Ord a => [a] -> [a]
uniques = Set.toList . Set.fromList
