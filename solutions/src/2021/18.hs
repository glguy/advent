{-# Language BlockArguments, ViewPatterns, LambdaCase #-}
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

import Advent (getInputLines)
import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (tails)
import Text.ParserCombinators.ReadP (ReadP, char, munch1, readP_to_S)

-- | >>> :main
-- 3551
-- 4555
main :: IO ()
main =
 do inp <- map parse <$> getInputLines 18
    print (magnitude (foldl1 add inp))
    print (maximum [magnitude (add x y) `max` magnitude (add y x)
                   | x:ys <- tails inp, y <- ys])

-- * Snailfish operations

-- | Add two expressions and reduce them
add :: Tree Int -> Tree Int -> Tree Int
add x y = reduce (x :+: y)

-- | Reduce an expression until it won't reduce
reduce :: Tree Int -> Tree Int
reduce x = maybe x reduce (explode <$> unstable x <|> split x)

-- | Find the left-most pair at depth 4.
unstable :: Tree a -> Maybe (a, a, Zip a)
unstable = go 4 []
  where
    go :: Int -> Zip a -> Tree a -> Maybe (a, a, Zip a)
    go 0 z (Leaf l :+: Leaf r) = Just (l, r, z)
    go 0 _ _ = Nothing
    go d z (l :+: r) = go (d-1) ((R,r):z) l <|> go (d-1) ((L,l):z) r
    go _ _ _ = Nothing

-- Add the left and right components to the nearest number
-- on the left and right respectively. Replace the hole with
-- a zero.
explode :: (Int, Int, Zip Int) -> Tree Int
explode (l, r, z)
  = fromZip (Leaf 0)
  $ appUp L (appR (l+))
  $ appUp R (appL (r+)) z

-- | Replace the first number with value 10 or more with a pair
-- of it divided in half rounding first down then up.
split :: Tree Int -> Maybe (Tree Int)
split (Leaf x) | x >= 10 = case quotRem x 2 of (q,r) -> Just (Leaf q :+: Leaf (q+r))
split (l :+: r) = (:+: r) <$> split l <|> (l :+:) <$> split r
split _ = Nothing

-- | Compute the /magnitude/ of an expression
--
-- >>> magnitude (parse "[9,1]")
-- 29
--
-- >>> magnitude (parse "[[1,2],[[3,4],5]]")
-- 143
magnitude :: Tree Int -> Int
magnitude (Leaf x) = x
magnitude (l :+: r) = 3 * magnitude l + 2 * magnitude r

-- * Binary trees

-- | A binary tree with data at the leaves
data Tree a
  = Tree a :+: Tree a -- ^ tuple
  | Leaf a  -- ^ regular number
  deriving Show

-- * Tree zippers

-- | Marks the side of a tree node constructor that
-- we know the subtree of.
data Side = L | R deriving (Eq, Show)

-- | A hole in a binary tree. Rebuild the tree with 'fromZip'
type Zip a = [(Side, Tree a)]

-- | Rebuild a tree given a zipper and the value to put in the hole.
fromZip :: Tree a -> Zip a -> Tree a
fromZip = foldl \x -> \case
  (L, l) -> l :+: x
  (R, r) -> x :+: r

-- | Apply the given function to the nearest parent
-- sibling on the given side.
appUp :: Side -> (Tree a -> Tree a) -> Zip a -> Zip a
appUp L f ((L,l):zs) = (L, f l):zs
appUp R f ((R,r):zs) = (R, f r):zs
appUp h f (z    :zs) = z : appUp h f zs
appUp _ _ []         = []

-- | Apply a function to the left-most leaf
appL :: (a -> a) -> Tree a -> Tree a
appL f (l :+: r) = appL f l :+: r
appL f (Leaf x) = Leaf (f x)

-- | Apply a function to the rightmost leaf
appR :: (a -> a) -> Tree a -> Tree a
appR f (l :+: r) = l :+: appR f r
appR f (Leaf x) = Leaf (f x)

-- * Parsing

-- | Parse an expression
--
-- >>> parse "[[[[0,9],2],3],4]"
-- (((Leaf 0 :+: Leaf 9) :+: Leaf 2) :+: Leaf 3) :+: Leaf 4
parse :: String -> Tree Int
parse (readP_to_S (pTree pInt) -> [(x,_)]) = x
parse _ = error "bad input"

-- | Unsigned 'Int' parser
pInt :: ReadP Int
pInt = read <$> munch1 isDigit

-- | ReadP expression parser
pTree :: ReadP a -> ReadP (Tree a)
pTree pLeaf = tuple <|> number
  where
    tuple = (:+:) <$ char '[' <*> pTree pLeaf <* char ',' <*> pTree pLeaf <* char ']'
    number = Leaf <$> pLeaf
