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

>>> :{
:main +
    "$ cd /\n\
    \$ ls\n\
    \dir a\n\
    \14848514 b.txt\n\
    \8504156 c.dat\n\
    \dir d\n\
    \$ cd a\n\
    \$ ls\n\
    \dir e\n\
    \29116 f\n\
    \2557 g\n\
    \62596 h.lst\n\
    \$ cd e\n\
    \$ ls\n\
    \584 i\n\
    \$ cd ..\n\
    \$ cd ..\n\
    \$ cd d\n\
    \$ ls\n\
    \4060174 j\n\
    \8033020 d.log\n\
    \5626152 d.ext\n\
    \7214296 k\n"
:}
95437
24933642

-}
module Main where

import Data.List (tails)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import Data.Map qualified as Map
import Data.Tree (Tree(..))
import Data.Tree qualified as Tree

import Advent (format)

-- | File paths are represented as a list of directory components
-- in reverse order to make it quicker to manipulate.
type Path = [String]

-- | The input is a list of directory change commands and directory listings.
type Input = [Either String [Either (Int, String) String]]

-- |
-- >>> :main
-- 1477771
-- 3579501
main :: IO ()
main =
 do input <- [format|2022 7
      ($ cd %s%n
      |$ ls%n
      (%u %s%n
      |dir %s%n)*)*|]

    -- the root directory will always be the first entry
    let totalSizes = lsToTotalSizes (summarizeLs [] input)

    -- part 1
    print (sum [n | n <- totalSizes, n <= 100_000])

    -- part 2
    let minNeeded = head totalSizes + 30_000_000 - 70_000_000
    print (minimum [n | n <- totalSizes, n >= minNeeded])

-- | Generate all the total sizes of directories given the list of
-- immediate sizes of directories.
lsToTotalSizes :: [(Path, Int)] -> [Int]
lsToTotalSizes dirs = Map.elems (Map.fromListWith (+) [(d',n) | (d,n) <- dirs, d' <- tails d])

-- | Given a list of cd commands and directory lists, generate a list
-- of directories and total size of immediate files.
summarizeLs ::
  Path {- ^ current working directory -} ->
  Input ->
  [(Path, Int)] {- ^ list of directories and their immediate sizes -}
summarizeLs _   [] = []
summarizeLs _   (Left "/"  : xs) = summarizeLs [] xs
summarizeLs cwd (Left ".." : xs) = summarizeLs (drop 1 cwd) xs
summarizeLs cwd (Left dir  : xs) = summarizeLs (dir : cwd) xs
summarizeLs cwd (Right ls  : xs) = (cwd, sum [n | Left (n,_) <- ls]) : summarizeLs cwd xs
