{-# Language BlockArguments, LambdaCase #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/5>

Password validation problems.

-}
module Main where

import Advent (getInputLines, countBy)
import Data.List (isInfixOf, tails)

main :: IO ()
main =
  do strs <- getInputLines 5
     print (countBy part1 strs)
     print (countBy part2 strs)

part1 :: String -> Bool
part1 str = threeVowels str && hasDouble str && noProhibited str

part2 :: String -> Bool
part2 str = pairTwice str && nearby str

threeVowels :: String -> Bool
threeVowels = not . null . drop 2 . filter (`elem` "aeiou")

hasDouble :: String -> Bool
hasDouble =
  search \case
    x:y:_ -> x == y
    _     -> False

noProhibited :: String -> Bool
noProhibited str = not (any (`isInfixOf` str) ["ab","cd","pq","xy"])

search :: (String -> Bool) -> String -> Bool
search p = any p . tails

pairTwice :: String -> Bool
pairTwice =
  search \case
    x:y:z -> [x,y] `isInfixOf` z
    _     -> False

nearby :: String -> Bool
nearby =
  search \case
    w:_:y:_ -> w == y
    _       -> False
