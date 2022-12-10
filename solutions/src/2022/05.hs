{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/5>

-}
module Main where

import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Map (Map)
import Data.Map qualified as Map

import Advent (format)

main :: IO ()
main =
 do (toppart, labels, commands) <- [format|2022 5
        ((   |[%c])& %n)*
        ( %c )& %n
        %n
        (move %u from %c to %c%n)*|]
    let stacks = Map.fromList (zip labels (map catMaybes (transpose toppart)))
    let solve f = map head (Map.elems (foldl (apply f) stacks commands))
    putStrLn (solve reverse)
    putStrLn (solve id     )

apply :: Ord k => ([a] -> [a]) -> Map k [a] -> (Int, k, k) -> Map k [a]
apply f stacks (n, fr, to) =
    case Map.alterF (traverse (splitAt n)) fr stacks of
        (a, m) -> Map.adjust (f a ++) to m
