{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/14>

-}
module Main where

import Control.Monad (foldM)
import Data.List (find, foldl')
import Data.Set (Set)
import Data.Set qualified as Set

import Advent (format)
import Advent.Coord (below, coordRow, left, right, Coord(..))

-- |
-- >>> :main
-- 644
-- 27324
main :: IO ()
main = do
    input <- [format|2022 14 ((%u,%u)&( -> )%n)*|]
    let world = Set.fromList (concatMap segs input)
        limit = 1 + maximum [ y| C y _ <- Set.toList world]

    case fillFrom1 limit world top of
      Right {} -> fail "no solution"
      Left world1 -> print (Set.size world1 - Set.size world)
    
    print (Set.size (fillFrom2 limit world top) - Set.size world)

-- | The entry point of sand at @500,0@
top :: Coord
top = C 0 500

fillFrom1 :: Int -> Set Coord -> Coord -> Either (Set Coord) (Set Coord)
fillFrom1 limit world here
  | limit < coordRow here = Left world
  | Set.member here world = Right world
  | otherwise = Set.insert here <$> foldM (fillFrom1 limit) world
                  [below here, left (below here), right (below here)]

fillFrom2 :: Int -> Set Coord -> Coord -> Set Coord
fillFrom2 limit world here
  | Set.member here world || limit < coordRow here = world
  | otherwise = foldl' (fillFrom2 limit) (Set.insert here world)
                  [below here, left (below here), right (below here)]

-- Turning line segments into sets of coordinates

segs :: [(Int,Int)] -> [Coord]
segs (x:y:z) = seg x y ++ segs (y:z)
segs [(x,y)] = [C y x]
segs []      = []

seg :: (Int,Int) -> (Int,Int) -> [Coord]
seg (a,b) (c,d)
  | a == c    = [C y a | y <- [min b d .. max b d]]
  | b == d    = [C d x | x <- [min a c .. max a c]]
  | otherwise = error "unexpected input"
