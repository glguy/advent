{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/3>

-}
module Main where

import Data.Char (isLower, ord)
import Data.List (intersect)

import Advent (format, chunks, ordNub)

main :: IO ()
main =
 do input <- [format|2022 3 (%s%n)*|]
    print (sum (map score (map halves input)))
    print (sum (map score (chunks 3 input)))

halves :: String -> [String]
halves xs = chunks (length xs `div` 2) xs

score :: [String] -> Int
score = sum . map priority . ordNub . foldl1 intersect

priority :: Char -> Int
priority x
  | isLower x = ord x - ord 'a' + 1
  | otherwise = ord x - ord 'A' + 27