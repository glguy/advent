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

import Data.List (find)
import Data.Set (Set)
import Data.Set qualified as Set
import Advent
import Advent.Coord

-- |
-- >>> :main
-- 644
-- 27323
main :: IO ()
main = do
    input <- [format|2022 14 ((%u,%u)&( -> )%n)*|]
    let world = Set.fromList [x | xs <- input, x <- segs (map toCoord xs)]
        limit = 1 + maximum [ y| C y _ <- Set.toList world]
    print (part1 limit world)
    print (part2 limit world)

top :: Coord
top = C 0 500

part1 :: Int -> Set Coord -> Int
part1 limit = go 0
  where
    go n w
      | coordRow c == limit = n
      | otherwise = go (n+1) (Set.insert c w)
      where c = walk limit w top

part2 :: Int -> Set Coord -> Int
part2 limit = go 0
  where
    go n w
      | c == top = n+1
      | otherwise = go (n+1) (Set.insert c w)
      where c = walk limit w top

toCoord :: (Int,Int) -> Coord
toCoord (x,y) = C y x

segs :: [Coord] -> [Coord]
segs (x:y:z) = seg x y ++ segs (y:z)
segs [x] = [x]
segs [ ] = [ ]

seg :: Coord -> Coord -> [Coord]
seg (C a b) (C c d)
  | a == c    = [C a x | x <- [min b d .. max b d]]
  | b == d    = [C x d | x <- [min a c .. max a c]]
  | otherwise = error "unexpected input"

walk :: Int -> Set Coord -> Coord -> Coord
walk cutoff world here
  | coordRow here == cutoff = here
  | Just here' <- find (`Set.notMember` world) [below here, left (below here), right (below here)]
    = walk cutoff world here'
  | otherwise = here