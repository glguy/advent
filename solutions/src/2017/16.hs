{-# Language QuasiQuotes, DataKinds, NumDecimals, LambdaCase, BlockArguments #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/16>

Day 16 defines a language of renaming and permutations and asks us
to iterate the program one billion times!

The key to this solution is that 'stimes' can used repeated squaring
to efficiently compute the result of multiplication by a large factor.

There are two kind of dance moves: permutation of positions, and renamings
of dancers. These two kinds of moves commute with each other. Both renamings
and permutations can efficiently compose, so we represent a dance as a single
renaming and a single permutation. This representation means that our dance
combination operation ('<>') is associative, as required for dances to be a
'Monoid' because the component permutations themselves support an associative
composition.

$setup
>>> :set -XDataKinds -XNumDecimals

>>> let steps = map danceStep (parseInput "s1,x3/4,p3/b\n")
>>> let dance = mconcat steps :: Dance 5
>>> mapM_ (putStrLn . runDance) (scanl (<>) dance steps)
baedc
cbaed
cbade
ceadb

>>> putStrLn (runDance (stimes (1e9 :: Int) dance))
abcde

-}
module Main (main) where

import Advent.Format (format)
import Advent.Permutation (Permutation, rotateRight, runPermutation, swap)
import Data.Semigroup (Dual(..), stimes)
import Data.Char (chr, ord)
import GHC.TypeLits (KnownNat)

[format|(s%d|x%d/%d|p%c/%c)&,%n|]

-- | Print the solutions to both parts of the day 16 problem. The input
-- file can be overridden via command-line arguments.
--
-- >>> :main
-- fnloekigdmpajchb
-- amkjepdhifolgncb
main :: IO ()
main =
 do dance <- foldMap danceStep <$> getInput 2017 16
    putStrLn (runDance (dance :: Dance 16))
    putStrLn (runDance (stimes (1e9 :: Int) dance))

-- | Convert a single input instruction into a dance permutation.
danceStep :: KnownNat n => Either (Either Int (Int, Int)) (Char, Char) -> Dance n
danceStep = \case
  Left (Left n)      -> spinDance n
  Left (Right (x,y)) -> swapDance x y
  Right (x,y)        -> partDance x y

-- | Map the numbers starting at @0@ to the letters starting at @a@.
--
-- >>> intToLetter <$> [0..3]
-- "abcd"
intToLetter :: Int -> Char
intToLetter i = chr (i + ord 'a')

-- | Map the letters starting at @a@ to the numbers starting at @0@.
--
-- >>> letterToInt <$> ['a'..'d']
-- [0,1,2,3]
letterToInt :: Char -> Int
letterToInt c = ord c - ord 'a'

-- | A dance is a renaming of dancers and a permutation of their positions
type Dance n = (Dual (Permutation n), Permutation n) -- ^ renaming, permutation

-- | Compute the final position of the dancers given a dance where
-- dancers start in order.
--
-- >>> let example = spinDance 1 <> swapDance 3 4 <> partDance 'e' 'b' :: Dance 5
-- >>> runDance example
-- "baedc"
-- >>> runDance (stimes 2 example)
-- "ceadb"
runDance :: KnownNat n => Dance n -> String
runDance (Dual r, p) = runPermutation intToLetter (r <> p)

-- | The spin dance where all dancers move some number of positions
-- to the right.
--
-- >>> runDance (spinDance 0 :: Dance 3)
-- "abc"
-- >>> runDance (spinDance 1 :: Dance 3)
-- "cab"
spinDance :: KnownNat n => Int -> Dance n
spinDance n = (mempty, rotateRight n)

-- | The swap dance where dancers in the two positions trade places.
--
-- >>> runDance (swapDance 0 1 :: Dance 3)
-- "bac"
-- >>> runDance (swapDance 0 1 <> swapDance 1 2 :: Dance 3)
-- "bca"
swapDance :: KnownNat n => Int -> Int -> Dance n
swapDance x y = (mempty, swap x y)

-- | The parter dance where the two named dancers changes positions.
--
-- >>> runDance (partDance 'a' 'b' :: Dance 3)
-- "bac"
-- >>> runDance (partDance 'a' 'b' <> partDance 'a' 'c' :: Dance 3)
-- "bca"
partDance :: KnownNat n => Char -> Char -> Dance n
partDance x y = (Dual (swap (letterToInt x) (letterToInt y)), mempty)
