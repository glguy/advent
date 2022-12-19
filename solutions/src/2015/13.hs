{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/13>

>>> :{
:main +
  "Alice would gain 54 happiness units by sitting next to Bob.\n\
  \Alice would lose 79 happiness units by sitting next to Carol.\n\
  \Alice would lose 2 happiness units by sitting next to David.\n\
  \Bob would gain 83 happiness units by sitting next to Alice.\n\
  \Bob would lose 7 happiness units by sitting next to Carol.\n\
  \Bob would lose 63 happiness units by sitting next to David.\n\
  \Carol would lose 62 happiness units by sitting next to Alice.\n\
  \Carol would gain 60 happiness units by sitting next to Bob.\n\
  \Carol would gain 55 happiness units by sitting next to David.\n\
  \David would gain 46 happiness units by sitting next to Alice.\n\
  \David would lose 7 happiness units by sitting next to Bob.\n\
  \David would gain 41 happiness units by sitting next to Carol.\n"
:}
330
286

-}
module Main where

import Advent
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

data E = Egain | Elose deriving Show

stageTH

data Edge = Edge String String
  deriving (Eq, Ord)

edge :: String -> String -> Edge
edge a b
  | a < b     = Edge a b
  | otherwise = Edge b a

-- |
-- >>> :main
-- 733
-- 725
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
