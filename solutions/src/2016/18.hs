{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/18>

-}
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
  do input <- head <$> getInputLines 2016 18
     print (problem input     40)
     print (problem input 400000)
