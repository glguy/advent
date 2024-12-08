{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/8>

-}
module Main (main) where

import Advent (getInputMap)
import Advent.Coord (coordRow, coordCol, Coord (C))
import Control.Monad (guard)
import Data.List (nub, tails)
import Data.Map qualified as Map

-- | >>> :main
-- 303
-- 1045
main :: IO ()
main =
 do input <- getInputMap 2024 8
    let locs = Map.filter (\x -> length x > 1)
             $ Map.fromListWith (++) [(v, [k]) | (k,v) <- Map.assocs input, v /= '.']
    print $ length $ nub $
     do posns <- Map.elems locs
        x:ys <- tails posns
        y <- ys
        node <- [y + (y - x), x + (x - y)]
        guard (Map.member node input)
        pure node
    print $ length $ nub $ concat (Map.elems locs) ++
     do posns <- Map.elems locs
        x:ys <- tails posns
        y <- ys
        z <- Map.keys input
        guard (inlined x y z)
        [z,x,y]

inlined :: Coord -> Coord -> Coord -> Bool
inlined x y z
  | coordCol x == coordCol z = coordCol x == coordCol y
  | otherwise = coordCol z /= coordCol y && slope z x == slope y x

slope :: Coord -> Coord -> Rational
slope (C y1 x1) (C y2 x2) = fromIntegral (y2-y1) / fromIntegral (x2-x1)
