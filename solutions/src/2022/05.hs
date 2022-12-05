{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-# OPTIONS_GHC -w #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/5>

-}
module Main where

import Data.Char (isDigit, digitToInt)
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as Map

import Advent (format)

main :: IO ()
main = do
    (toppart, commands) <- [format|2022 5 (%c+%n)*%n(move %u from %c to %c%n)*|]
    let columns = parseColumns toppart
    putStrLn $ map head $ Map.elems $ foldl (apply reverse) columns commands
    putStrLn $ map head $ Map.elems $ foldl (apply id     ) columns commands

apply :: Ord k => ([a] -> [a]) -> Map k [a] -> (Int, k, k) -> Map k [a]
apply f columns (n, fr, to) =
    let (a,b) = splitAt n (columns Map.! fr) in
    Map.adjust (f a ++) to (Map.insert fr b columns)

parseColumns :: [String] -> Map Char String
parseColumns strs =
    Map.fromList
        [(n, dropWhile (' '==) (reverse row))
            | n:row <- map reverse (transpose strs)
            , n /= ' ']