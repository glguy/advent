{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day  solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/25>

-}
module Main (main) where

import Advent.Coord (Coord(..), below, right)
import Advent.Input (getInputMap)
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 582
main :: IO ()
main =
 do inp <- getInputMap 25
    let C ny nx = 1 + maximum (Map.keys inp)
    let inp' = Map.filter (`elem` ">v") inp
    let steps = iterate (step ny nx) inp'
    print (length (evolution steps))

evolution :: Eq a => [a] -> [a]
evolution (x:y:_) | x == y = [x]
evolution (x:xs) = x : evolution xs
evolution [] = []

step :: Int -> Int -> Map Coord Char -> Map Coord Char
step ny nx = step1 ny nx 'v' below . step1 ny nx '>' right

step1 :: Int -> Int -> Char -> (Coord -> Coord) -> Map Coord Char -> Map Coord Char
step1 ny nx c f inp =
  Map.fromList [
    (if v == c && Map.notMember k' inp then k' else k, v) 
    | (k, v) <- Map.toList inp
    , let k' = fixup (f k)
    ]
  where
    fixup (C y x) = C (y `mod` ny) (x `mod` nx)
