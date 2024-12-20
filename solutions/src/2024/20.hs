{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/20>

-}
module Main (main) where

import Advent (getInputArray, arrIx)
import Advent.Coord (cardinal, Coord, manhattan)
import Advent.Search (astar, AStep(AStep))
import Data.Array.Unboxed (UArray, amap, assocs)
import Data.Map qualified as Map

main :: IO ()
main =
 do input <- getInputArray 2024 20
    let walls = amap ('#' /=) input
    let start : _ = [p | (p,'S') <- assocs input]
    let end   : _ = [p | (p,'E') <- assocs input]
    let optimals = Map.fromList (search walls end)
    let part n = length [()
                | (p1, True) <- assocs walls
                , (p2, True) <- assocs walls
                , let d = manhattan p1 p2
                , d > 0, d <= n
                , let c1 = optimals Map.! p1
                , let c2 = optimals Map.! p2
                , c1 - c2 - d >= 100
                ]
    print (part  2)
    print (part 20)

search :: UArray Coord Bool -> Coord -> [(Coord, Int)]
search input start = astar step start
  where
    step p = [ AStep p' 1 0 | p' <- cardinal p, True <- arrIx input p']
