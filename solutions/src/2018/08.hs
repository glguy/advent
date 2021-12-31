{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/8>

Parse a tree out of a list of integers and then answer a pair of queries
about the tree.

-}
{-# Language DeriveTraversable #-}
module Main (main) where

import Advent (format)
import Control.Monad (replicateM)
import Control.Monad.Trans.State.Strict (StateT(..))
import Data.List (uncons)
import Data.Maybe (fromMaybe)

-- | Print the answers to day 8
--
-- >>> :main
-- 42196
-- 33649
main :: IO ()
main =
 do input <- [format|8 %u& %n|]
    let Just (tree, []) = runStateT parseTree input
    print (sum tree)
    print (part2 tree)

-- | A tree can have children and metadata entries.
data Tree a = Tree [Tree a] [a] -- ^ children and metadata
  deriving (Functor, Foldable, Traversable, Show)

-- | Sum of metadata entries on leaf nodes and recursive call on
-- child nodes identified by indexes stored in metadata.
part2 :: Tree Int -> Int
part2 (Tree xs ys)
  | null xs   = sum ys
  | otherwise = sum [ sum (index (map part2 xs) (i-1)) | i <- ys, i > 0]

-- | list index returning Nothing on failure.
index :: [a] -> Int -> Maybe a
index xs n
  | a:_ <- drop n xs = Just a
  | otherwise        = Nothing

-- | Get the next integer
int :: StateT [Int] Maybe Int
int = StateT uncons

-- | Parse a tree from a list of integers
parseTree :: StateT [Int] Maybe (Tree Int)
parseTree =
 do n <- int
    m <- int
    a <- replicateM n parseTree
    b <- replicateM m int
    pure (Tree a b)
