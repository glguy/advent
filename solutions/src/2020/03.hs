{-# Language ImportQualifiedPost, ParallelListComp #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/3>

Sledding down a slope counting trees.

-}
module Main where

import Advent (countBy, getInputArray)
import Advent.Coord (Coord(C))
import Data.Array.Unboxed qualified as A

-- |
-- >>> :main
-- 240
-- 2832009600
main :: IO ()
main =
  do inp <- getInputArray 3
     print $ solve 3 1 inp
     print $ solve 1 1 inp
           * solve 3 1 inp
           * solve 5 1 inp
           * solve 7 1 inp
           * solve 1 2 inp

solve :: Int -> Int -> A.UArray Coord Char -> Int
solve dx dy grid
  = countBy (\c -> '#' == grid A.! c)
    [ C y (xlo + xoff `rem` width)
    | xoff <- [0, dx ..]
    | y    <- [ylo, ylo+dy .. yhi]]
  where
    width = xhi - xlo + 1
    (C ylo xlo, C yhi xhi) = A.bounds grid
