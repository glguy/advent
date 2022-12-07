{-# Language QuasiQuotes, ImportQualifiedPost, NumericUnderscores #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/7>

The immediate size of a directory is the list of the files
contained in it directly, while the total size includes the
files that are contained within subdirectories.

-}
module Main where

import Data.Map qualified as Map
import Data.List (tails)

import Advent (format)

-- | File paths are represented as a list of directory components
-- in reverse order to make it quicker to manipulate.
type Path = [String]

-- | The input is a list of directory change commands and directory listings.
type Input = [Either String [Either (Int, String) String]]

main :: IO ()
main =
 do input <- [format|2022 7
      ($ cd %s%n
      |$ ls%n
      (%u %s%n
      |dir %s%n)*)*|]

    let dirs = summarizeLs [] input
    let totalSizes = lsToTotalSizes dirs

    -- part 1
    print (sum [n | n <- totalSizes, n <= 100_000])

    -- part 2
    let totalUsed = sum [n | (_,n) <- dirs]
    print $ minimum [freed | freed <- totalSizes, 70_000_000 - totalUsed >= 30_000_000 - freed ]

-- | Generate all the total sizes that directories have given the list of
-- just the immediate sizes.
lsToTotalSizes :: [(Path, Int)] -> [Int]
lsToTotalSizes dirs = Map.elems (Map.fromListWith (+) [(d',n) | (d,n) <- dirs, d' <- tails d])

-- | Given a list of cd commands and directory lists, generate a list
-- of directories and total size of immediate files.
summarizeLs ::
  Path {- ^ current working directory components -} ->
  Input ->
  [(Path, Int)] {- ^ list of directories and their immediate sizes -}
summarizeLs _   [] = []
summarizeLs _   (Left "/"   : xs) = summarizeLs [] xs
summarizeLs cwd (Left ".."  : xs) = summarizeLs (drop 1 cwd) xs
summarizeLs cwd (Left dir   : xs) = summarizeLs (dir : cwd) xs
summarizeLs cwd (Right list : xs) = (cwd, sum [n | Left (n,_) <- list]) : summarizeLs cwd xs
