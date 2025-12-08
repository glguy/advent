{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/8>

-}
module Main (main) where

import Advent (format)
import Advent.Coord3 ( Coord3(C3) )
import Advent.DisjointSet (newDisjointSet, setRepresentative, setSize, unifySets, DisjointSet)
import Data.List ( sort, sortOn, tails )
import Data.Maybe ( catMaybes )

-- | >>> :main
-- 244188
-- 8361881885
main :: IO ()
main =
 do input <- [format|2025 8 (%u,%u,%u%n)*|]
    let n = length input

    -- Pairs of junction identifiers and coordinates
    let pairs = [ (i, j, C3 x1 x2 x3, C3 y1 y2 y3)
                | (i, (x1, x2, x3)) : xs <- tails (zip [1..] input)
                , (j, (y1, y2, y3))      <- xs]

    -- Pairs sorted by straight line distance
    let pairs' = sortOn dist2 pairs

    ds <- newDisjointSet (1, n)
    let (p1, p2) = splitAt 1000 pairs'
    
    -- Do the first 1000 connections and compute the part 1 answer
    n1 <- loop (length input) ds p1
    print =<< part1Answer ds n
    
    -- Keep going and compute the part 2 answer
    loop n1 ds p2
    pure ()

loop ::
  Int {- ^ disjoint circuit count -} ->
  DisjointSet Int ->
  [(Int, Int, Coord3, Coord3)] {- ^ pairs of junction boxes in distance order -} ->
  IO Int {- ^ number of disjoint circuits remaining -}
loop n ds [] = pure n
loop n ds ((i, j, x, y):xs) =
  do i' <- setRepresentative ds i
     j' <- setRepresentative ds j
     if i' == j' then loop n ds xs
       else do
        unifySets ds i' j'
        if n == 2 then 1 <$ print (part2Answer x y)
                  else loop (n-1) ds xs

-- | Distance-squared between the two points
dist2 :: (a, b, Coord3, Coord3) -> Int
dist2 (_, _, C3 x1 x2 x3, C3 y1 y2 y3) = sq (x1-y1) + sq (x2-y2) + sq (x3-y3)
  where sq x = x*x

part1Answer :: DisjointSet Int -> Int -> IO Int
part1Answer ds n =
 do sizes <- mapM (\x -> do
      x' <- setRepresentative ds x
      if x' == x then do
        n <- setSize ds x'
        pure (Just n)
      else pure Nothing) [1 .. n]
    pure (product (take 3 (reverse (sort (catMaybes sizes)))))

-- | Product of the x-coordinates of the last connection needed.
part2Answer :: Coord3 -> Coord3 -> Int
part2Answer (C3 x _ _) (C3 y _ _) = x * y
