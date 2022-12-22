{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost, LambdaCase, BangPatterns #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/22>

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.List (foldl')

import Advent (format, stageTH)
import Advent.Coord

data D = DL | DR

stageTH

-- |
-- >>> :main
-- 162186
-- 55267
main :: IO ()
main =
 do (rawmap, path) <- [format|2022 22 (( |.|#)*!%n)*%n(%u|@D)*%n|]
    let board = Map.filter (' ' /=) (Map.fromList (coordLines rawmap))
    let start = minimum (Map.keys board)
    print (score (go1 path start board))
    print (score (go2 path start board))

score :: (Coord, Coord) -> Int
score (C y x, dir) = 1000 * (y+1) + 4 * (x+1) + faceval
  where
    faceval
      | dir == east  = 0
      | dir == south = 1
      | dir == west  = 2
      | dir == north = 3
      | otherwise = error "faceval: bad direction"

go1 :: [Either Int D] -> Coord -> Map Coord Char -> (Coord, Coord)
go1 commands start board = foldl' f (start, east) commands
  where
    f (!here, !dir) = \case
      Left  n  -> (walk1 n dir here board, dir)
      Right DL -> (here, turnLeft dir)
      Right DR -> (here, turnRight dir)

walk1 :: Int -> Coord -> Coord -> Map Coord Char -> Coord
walk1 0 _ here _ = here
walk1 n dir here board
  | board Map.! here' == '#' = here
  | otherwise = walk1 (n-1) dir here' board
  where
    here'
      | Map.member (here+dir) board = here+dir
      | otherwise = last (takeWhile (`Map.member` board) (iterate (subtract dir) here))

go2 :: [Either Int D] -> Coord -> Map Coord Char -> (Coord, Coord)
go2 commands start board = foldl' f (start, east) commands
  where
    f (!here, !dir) = \case
      Left n   -> walk2 n dir here board
      Right DL -> (here, turnLeft dir)
      Right DR -> (here, turnRight dir)

walk2 :: Int -> Coord -> Coord -> Map Coord Char -> (Coord, Coord)
walk2 0 dir here _ = (here,dir)
walk2 n dir here board
  | board Map.! here' == '#' = (here,dir)
  | otherwise = walk2 (n-1) dir' here' board
  where
   (here', dir') =
      let fr = coordRow here `mod` 50
          fc = coordCol here `mod` 50
          fr' = 49 - fr in
      case (cubeface here, cubeface (here+dir)) of
         (_,y) | -1 /= y -> (here+dir, dir)

         (1,_) | dir == north -> (C (150 + fc ) 0,east)
         (1,_) | dir == west  -> (C (100 + fr') 0, east)

         (2,_) | dir == north -> (C 199         fc, north)
         (2,_) | dir == east  -> (C (100 + fr') 99, west)
         (2,_) | dir == south -> (C ( 50 + fc ) 99, west)

         (3,_) | dir == east -> (C  49 (100 + fr), north)
         (3,_) | dir == west -> (C 100 fr        , south)

         (4,_) | dir == east  -> (C fr'        149, west)
         (4,_) | dir == south -> (C (150 + fc)  49, west)

         (5,_) | dir == north -> (C (50 + fc) 50, east)
         (5,_) | dir == west  -> (C fr'       50, east)

         (6,_) | dir == east  -> (C 149 ( 50 + fr), north)
         (6,_) | dir == south -> (C   0 (100 + fc), south)
         (6,_) | dir == west  -> (C   0 ( 50 + fr), south)

         (a,b) -> error (show (a,b, dir))

cubeface :: Coord -> Int
cubeface (C y x) =
   case (div y 50, div x 50) of
      (0,1) -> 1
      (0,2) -> 2
      (1,1) -> 3
      (2,0) -> 5
      (2,1) -> 4
      (3,0) -> 6
      _     -> -1
