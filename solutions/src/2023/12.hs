{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/12>

-}
module Main where

import Advent (format)
import Advent.Memo (memo2)
import Data.List (intercalate)

-- |
--
-- >>> :main
-- 6871
-- 2043098029844
main :: IO ()
main =
 do input <- [format|2023 12 (%s %d&,%n)*|]
    print (sum [match g s | (s,g) <- input])
    print (sum [match (concat (replicate 5 g)) (unfoldSprings s) | (s,g) <- input])

unfoldSprings :: String -> String
unfoldSprings = intercalate "?" . replicate 5

match :: [Int] -> [Char] -> Int
match = memo2 match'
  where
    match' [] xs
      | all (`elem` ".?") xs = 1
      | otherwise = 0
    match' _ [] = 0

    match' (n:ns) ('.':xs) = match (n:ns) xs
    match' (n:ns) ('#':xs) =
      case splitAt (n-1) xs of
        (a,x:b) | length a == (n-1), all (`elem` "#?") a, x `elem` "?." -> match ns b
        (a,[]) | length a == (n-1), all (`elem` "#?") a -> match ns []
        _ -> 0
    match' (n:ns) ('?':xs) = match (n:ns) ('.':xs) + match (n:ns) ('#':xs)
    match' a b = error (show (a,b))
