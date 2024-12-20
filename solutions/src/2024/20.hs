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
import Advent.Coord (Coord(C), cardinal, manhattan, coordRow, coordCol)
import Advent.Search (dfs)
import Data.Array.Unboxed (UArray, amap, assocs, accumArray, bounds)
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
        pathArray = accumArray (\_ e -> e) (-1) (bounds open) (zip path [0..]) :: UArray Coord Int
        (C loy lox, C hiy hix) = bounds input
        cheats    = [ d
                    | (p1, c1) <- zip path [0..]
                    , y2 <- [max loy (coordRow p1 - 20) .. min hiy (coordRow p1 + 20)]
                    , let dy = abs (coordRow p1 - y2)
                    , x2 <- [max lox (coordCol p1 - 20 + dy) .. min hix (coordCol p1 + 20 - dy)]
                    , let dx = abs (coordCol p1 - x2)
                    , let d = dx + dy
                    , c2 <- arrIx pathArray (C y2 x2)
                    , c2 - c1 >= 100 + d
                    ]
    print (count 2 cheats)
    print (length cheats)
