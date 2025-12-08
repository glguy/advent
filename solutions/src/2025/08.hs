{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/8>

>>> :{
:main +
"prefix=10
162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
"
:}
40
25272

-}
module Main (main) where

import Advent (format)
import Advent.Coord3 ( Coord3(C3) )
import Advent.DisjointSet (newDisjointSet, setRepresentative, setSize, unifySets, DisjointSet)
import Data.List ( sort, sortOn, tails, sortBy )
import Data.Maybe ( catMaybes, fromMaybe )

-- | >>> :main
-- 244188
-- 8361881885
main :: IO ()
main =
 do (mbPrefixLen, input) <- [format|2025 8 (prefix=%u%n|)(%u,%u,%u%n)*|]
    let prefixLen = fromMaybe 1000 mbPrefixLen
    let n = length input

    -- Pairs of junction identifiers and coordinates sorted by distance
    let pairs = sortOn dist2
                [ (i, j, C3 x1 x2 x3, C3 y1 y2 y3)
                | (i, (x1, x2, x3)) : xs <- tails (zip [1..] input)
                , (j, (y1, y2, y3))      <- xs]

    ds <- newDisjointSet (1, n)
    let (p1, p2) = splitAt prefixLen pairs

    -- Do the first 1000 (unless overridden) connections and compute the part 1 answer
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
 do success <- unifySets ds i j
    if success then
      if n == 2 then
        1 <$ print (part2Answer x y)
      else
        loop (n-1) ds xs
    else
      loop n ds xs

-- | Distance-squared between the two points
dist2 :: (a, b, Coord3, Coord3) -> Int
dist2 (_, _, x, y) = sq a + sq b + sq c
  where
    C3 a b c = x - y
    sq a = a * a

-- | Returns the size of each connected set.
setSizes :: DisjointSet Int -> Int -> IO [Int]
setSizes ds n = catMaybes <$> mapM rootSize [1 .. n]
  where
    rootSize x =
     do x' <- setRepresentative ds x
        if x' == x then
          Just <$> setSize ds x'
        else
          pure Nothing

part1Answer :: DisjointSet Int -> Int -> IO Int
part1Answer ds n =
 do sizes <- setSizes ds n
    pure (product (take 3 (sortBy (flip compare) sizes)))

-- | Product of the x-coordinates of the last connection needed.
part2Answer :: Coord3 -> Coord3 -> Int
part2Answer (C3 x _ _) (C3 y _ _) = x * y
