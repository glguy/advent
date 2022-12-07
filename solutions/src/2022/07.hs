{-# Language QuasiQuotes, ImportQualifiedPost, NumericUnderscores #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/7>

-}
module Main where

import Data.Map qualified as Map
import Data.List (inits)

import Advent (format)

-- | File paths are represented as a list of directory components
type Path = [String]

main :: IO ()
main =
 do input <- [format|2022 7
      ($ cd %s%n
      |$ ls%n
      (%u %s%n
      |dir %s%n)*)*|]

    let allFiles = simulate [] input
    let dirSizes = Map.elems (Map.fromListWith (+) [(d',n) | (d,n) <- allFiles, d' <- inits d])

    -- part 1
    print (sum [n | n <- dirSizes, n <= 100_000])

    -- part 2
    let totalUsed = sum [n | (_,n) <- allFiles]
    print $ minimum [freed | freed <- dirSizes, 70_000_000 - totalUsed >= 30_000_000 - freed ]

-- | Given a list of cd commands and directory lists, generate a list
-- of files contained alongside the file sizes.
simulate ::
  Path {- ^ current working directory components -} ->
  [Either String [Either (Int, String) String]] {- ^ list of either a cd or a directory listing -} ->
  [(Path, Int)] {- ^ list of files and their sizes -}
simulate _   [] = []
simulate _   (Left "/"   : xs) = simulate [] xs
simulate cwd (Left ".."  : xs) = simulate (init cwd) xs
simulate cwd (Left dir   : xs) = simulate (cwd ++ [dir]) xs
simulate cwd (Right list : xs) = (cwd, sum [n | Left (n,_) <- list]) : simulate cwd xs