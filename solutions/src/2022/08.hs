{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/8>

>>> :main + "30373\n25512\n65332\n33549\n35390\n"
21
8

-}
module Main where

import Data.Array.Unboxed (Ix(inRange, range), UArray, IArray(..), (!))

import Advent (getInputArray, countBy)
import Advent.Coord (above, below, left, right, Coord)

-- |
-- >>> :main
-- 1690
-- 535680
main :: IO ()
main =
 do input <- getInputArray 2022 08
    print (countBy (isEdgeVisible input) (range (bounds input)))
    print (maximum (map (scenicScore input) (range (bounds input))))

-- | Return the list of elements in the array starting
outToEdge ::
    UArray Coord Char {- ^ array -} ->
    Coord             {- ^ starting coordinate -} ->
    (Coord -> Coord)  {- ^ coordinate step function -} ->
    [Char]            {- ^ list of elements out to the edge of the array -}
outToEdge a c dir = [a ! i | i <- takeWhile (inRange (bounds a)) (iterate dir c)]

sightLines ::
    UArray Coord Char {- ^ array -} ->
    Coord             {- ^ starting coordinate -} ->
    [[Char]]          {- ^ list of trees viewed in each cardinal direction -}
sightLines a c = map (outToEdge a c) [above,below,left,right]

isEdgeVisible :: UArray Coord Char -> Coord -> Bool
isEdgeVisible a c = any clearView (sightLines a c)

clearView :: [Char] -> Bool
clearView [] = error "clearView: empty list"
clearView (x:xs) = all (<x) xs

scenicScore :: UArray Coord Char -> Coord -> Int
scenicScore a c = product (map treesSeen (sightLines a c))

treesSeen :: [Char] -> Int
treesSeen [] = error "treesSeen: empty list"
treesSeen (x:xs) =
    case break (>= x) xs of
        (a,[])  -> length a
        (a,_:_) -> length a + 1
