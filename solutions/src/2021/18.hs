{-# Language OverloadedStrings, BlockArguments, ViewPatterns #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/18>

Today's problem had us perform manipulations on a tree-based
term language. It was made tricky because the problem asked
us to do things to the nearest left and right neighbors
of elements of our tree.

-}
module Main (main) where

import Advent.Input (getInputLines)
import Advent.ReadS (P(..), runP)
import Control.Applicative ((<|>))
import Data.List (foldl1', tails)

-- | >>> :main
-- 3551
-- 4555
main :: IO ()
main =
 do inp <- map parse <$> getInputLines 2021 18
    print (magnitude (foldl1' add inp))
    print (maximum [magnitude (add x y) `max` magnitude (add y x)
                   | x:ys <- tails inp, y <- ys])

-- * Snailfish operations

-- | Add two expressions and reduce them
add :: Tree -> Tree -> Tree
add x y = reduce (x :+: y)

-- | Reduce an expression until it won't reduce
reduce :: Tree -> Tree
reduce (explode -> x) = maybe x reduce (split x) 

-- | Explode /all/ the pairs at depth 4 from left to right.
explode :: Tree -> Tree
explode = down 0 []
  where
    down 4 z (Leaf l :+: Leaf r) = up 4 (sendUp L l (sendUp R r z)) (Leaf 0)
    down d z (l :+: r) | d < 4   = down (d+1) ((R,r):z) l
    down d z t                   = up d z t

    up _ []        t = t
    up d ((R,r):z) l = down d ((L,l):z) r
    up d ((L,l):z) r = up (d-1) z (l :+: r)

-- | Replace the first number with value 10 or more with a pair
-- of it divided in half rounding first down then up.
split :: Tree -> Maybe Tree
split (l :+: r) = (:+: r) <$> split l <|> (l :+:) <$> split r
split (Leaf x)
  | x >= 10, (q,r) <- quotRem x 2 = Just (Leaf q :+: Leaf (q+r))
  | otherwise                     = Nothing

-- | Compute the /magnitude/ of an expression
--
-- >>> magnitude (parse "[9,1]")
-- 29
--
-- >>> magnitude (parse "[[1,2],[[3,4],5]]")
-- 143
magnitude :: Tree -> Int
magnitude (Leaf x) = x
magnitude (l :+: r) = 3 * magnitude l + 2 * magnitude r

-- * Binary trees

-- | A binary tree with integers at the leaves
data Tree
  = Tree :+: Tree -- ^ tuple
  | Leaf !Int  -- ^ number
  deriving Show

-- * Tree zippers

-- | Marks the side of a tree node constructor that
-- we know the subtree of.
data Side = L | R deriving (Eq, Show)

-- | A hole in a binary tree. Rebuild the tree with 'fromZip'
type Zip = [(Side, Tree)]

-- | Apply the given function to the nearest parent
-- sibling on the given side.
sendUp :: Side -> Int -> Zip -> Zip
sendUp L x ((L,l):zs) = (L, sendR x l):zs
sendUp R x ((R,r):zs) = (R, sendL x r):zs
sendUp h x (z    :zs) = z : sendUp h x zs
sendUp _ _ []         = []

-- | Add a number to the left-most leaf
sendL :: Int -> Tree -> Tree
sendL x (l :+: r) = sendL x l :+: r
sendL x (Leaf y)  = Leaf (x + y)

-- | Add a number to the rightmost leaf
sendR :: Int -> Tree -> Tree
sendR x (l :+: r) = l :+: sendR x r
sendR x (Leaf y)  = Leaf (x + y)

-- * Parsing

-- | Parse a snailfish expression
parse :: String -> Tree
parse = runP pTree

-- | Tree parser from a leaf parser
pTree :: P Tree
pTree =
  Leaf <$> P reads <|>
  (:+:) <$ "[" <*> pTree <* "," <*> pTree <* "]"
