{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/20>

-}
module Main (main) where

import Advent (getInputArray, arrIx, count)
import Advent.Coord (Coord, cardinal, manhattan)
import Advent.Search (dfs)
import Data.Array.Unboxed (UArray, amap, assocs)
import Data.List (tails)

-- >>> :main
-- 1346
-- 985482
main :: IO ()
main =
 do input <- getInputArray 2024 20
    let open      = amap ('#' /=) input
        start : _ = [p | (p, 'S') <- assocs input]
        step p    = [p' | p' <- cardinal p, True <- arrIx open p']
        path      = dfs step start
        cheats    = [ d
                    | (p1, c1) : more <- tails (zip path [0..])
                    , (p2, c2)        <- drop 100 more
                    , let d = manhattan p1 p2, d <= 20
                    , c2 - c1 >= 100 + d
                    ]
    print (count 2 cheats)
    print (length cheats)
