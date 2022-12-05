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
import Data.Map (Map)
import Data.Map qualified as Map

import Advent (format)

main :: IO ()
main = do
    (toppart, commands) <- [format|2022 5 (%c+%n)*%n(move %u from %c to %c%n)*|]
    let stacks = parseStacks toppart
    let solve f = map head (Map.elems (foldl (apply f) stacks commands))
    putStrLn (solve reverse)
    putStrLn (solve id     )

apply :: Ord k => ([a] -> [a]) -> Map k [a] -> (Int, k, k) -> Map k [a]
apply f stacks (n, fr, to) =
    case Map.alterF (traverse (splitAt n)) fr stacks of
        (a, m) -> Map.adjust (f a ++) to m

parseStacks :: [String] -> Map Char String
parseStacks strs =
    Map.fromList
        [(n, dropWhile (' '==) (reverse row))
            | n:row <- map reverse (transpose strs)
            , n /= ' ']