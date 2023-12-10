{-# Language QuasiQuotes, TemplateHaskell, ImportQualifiedPost, LambdaCase #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/8>

-}
module Main (main) where

import Advent (format, stageTH)
import Control.Monad (unless)
import Data.List (findIndex)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

data D = DL | DR

stageTH

-- |
--
-- >>> :main
-- 20777
-- 13289612809129
main :: IO ()
main =
 do (steps, nodes) <- [format|2023 8 @D*%n%n(%s = %(%s, %s%)%n)*|]
    let nodes' = Map.fromList [(k, \case DL -> a; DR -> b) | (k,a,b) <- nodes]
    let mkPath start = scanl (nodes' Map.!) start (cycle steps)

    let path1  = mkPath "AAA"
    let paths2 = [mkPath start | (start, _, _) <- nodes, last start == 'A']

    unless (all (isTrivial part2 (length steps)) paths2) (fail "input not trivial")

    print (findIndex' part1 path1)
    print (foldl1 lcm (map (findIndex' part2) paths2))

part1, part2 :: String -> Bool
part1 x = "ZZZ" == x
part2 x = 'Z' == last x

-- Verifies that we actually got one of the trivial input files.
-- * The goal must be reached after a number of cycles that is a multiple of the steps
-- * The next goal must be the same as the previous and must be reachable in the
--   same number of steps
--
-- This guarantees that the path must actually cycle infinitely and that
-- there is exactly one goal state in the cycle.
isTrivial :: Eq a => (a -> Bool) -> Int -> [a] -> Bool
isTrivial p n xs =
  case [ (i,x) | (i, x) <- zip [0..] xs, p x ] of
    (i1,g1) : (i2,g2) : _ -> i1 `rem` n == 0 && 2 * i1 == i2 && g1 == g2
    _ -> False

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' p = fromJust . findIndex p
