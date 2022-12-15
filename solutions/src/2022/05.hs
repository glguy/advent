{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/5>

>>> :{
:main +
    "    [D]    \n\
    \[N] [C]    \n\
    \[Z] [M] [P]\n\
    \ 1   2   3 \n\
    \\n\
    \move 1 from 2 to 1\n\
    \move 3 from 1 to 3\n\
    \move 2 from 2 to 1\n\
    \move 1 from 1 to 2\n"
:}
CMZ
MCD

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
