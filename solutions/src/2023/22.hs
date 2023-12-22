{-# Language QuasiQuotes, MonadComprehensions, ImportQualifiedPost, DataKinds, GADTs #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/22>

>>> :{
:main +
"1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
"
:}
5
7

-}
module Main (main) where

import Advent (format, count, countBy)
import Advent.Box (intersectBox, Box(Pt, Dim), Box')
import Control.Parallel.Strategies (parMap, rseq)
import Data.List (delete, sortBy)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Ord (comparing)

-- | Parse the input boxes and print answers to both parts.
--
-- >>> :main
-- 441
-- 80778
main :: IO ()
main =
 do input <- [format|2023 22 (%d,%d,%d~%d,%d,%d%n)*|]
    let bricks = map toBrick input
    let sunk = lowerAll (zip [0..] bricks)
    let support = parMap rseq (countSupported sunk) sunk
    print (count 0 support)
    print (sum support)

countSupported :: [(Int, Box' 3)] -> (Int, Box' 3) -> Int
countSupported bricks brick =
  let bricks' = delete brick bricks in
  countBy id $
  Map.intersectionWith (/=) 
    (Map.fromList bricks')
    (Map.fromList (lowerAll bricks'))

lowerAll :: [(Int, Box' 3)] -> [(Int, Box' 3)]
lowerAll = foldl lowerOne [] . sortBy (comparing snd)
  where
    lowerOne xs (i,x)
      | Just x' <- lower x
      , all (\(_,y) -> isNothing (intersectBox x' y)) xs
      = lowerOne xs (i,x')
      
      | otherwise = (i,x):xs

lower :: Box' 3 -> Maybe (Box' 3)
lower (Dim z1 z2 (Dim x1 x2 (Dim y1 y2 Pt))) =
  [Dim (z1-1) (z2-1) (Dim x1 x2 (Dim y1 y2 Pt)) | z1 > 1]

toBrick :: (Int,Int,Int,Int,Int,Int) -> Box' 3
toBrick (x1,y1,z1,x2,y2,z2) = dim z1 z2 (dim x1 x2 (dim y1 y2 Pt))
  where
    dim a b = Dim (min a b) (max a b + 1)
