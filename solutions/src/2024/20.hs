{-# Language QuasiQuotes, BlockArguments #-}
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
import Advent.Coord (cardinal, Coord, manhattan)
import Advent.Search (astar, AStep(AStep))
import Data.Array.Unboxed (UArray, amap, assocs, accumArray, bounds, (!))
import Data.List (tails)

-- >>> :main
-- 1346
-- 985482
main :: IO ()
main =
 do input <- getInputArray 2024 20
    let open      = amap ('#' /=) input
        start : _ = [p | (p,'S') <- assocs input]
        end   : _ = [p | (p,'E') <- assocs input]
        (os,o:_)  = break (\(p,_) -> p == start) (search open end) -- take up to and including the start
        cheats    = [ d
                    | (p1,c1) : more <- tails (o:os)
                    , (p2,c2)        <- more
                    , let d = manhattan p1 p2, d <= 20
                    , abs (c1 - c2) >= 100 + d
                    ]
    print (count 2 cheats)
    print (length cheats)

search :: UArray Coord Bool -> Coord -> [(Coord, Int)]
search open = astar \p -> [AStep p' 1 0 | p' <- cardinal p, True <- arrIx open p']
