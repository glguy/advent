{-# Language QuasiQuotes, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/13>

>>> :{
:main +
    "[1,1,3,1,1]\n\
    \[1,1,5,1,1]\n\
    \\n\
    \[[1],[2,3,4]]\n\
    \[[1],4]\n\
    \\n\
    \[9]\n\
    \[[8,7,6]]\n\
    \\n\
    \[[4,4],4,4]\n\
    \[[4,4],4,4,4]\n\
    \\n\
    \[7,7,7,7]\n\
    \[7,7,7]\n\
    \\n\
    \[]\n\
    \[3]\n\
    \\n\
    \[[[]]]\n\
    \[[]]\n\
    \\n\
    \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
    \[1,[2,[3,[4,[5,6,0]]]],8,9]\n"
:}
13
140

-}
module Main where

import Control.Applicative ((<|>))
import Data.List (sortBy)
import Text.ParserCombinators.ReadP (ReadP, sepBy, readS_to_P)

import Advent (format)

-- | An arbitrarily nested list of lists of Int
data T = N Int | L [T] deriving (Eq, Read, Show)

-- | Parse a single nested lists of integer value. This parser uses
-- a single letter name to make it accessible from the format quasiquoter.
t :: ReadP T
t = L <$ "[" <*> t `sepBy` "," <* "]" <|>
    N <$> readS_to_P reads

-- |
-- >>> :main
-- 5340
-- 21276
main :: IO ()
main =
 do input <- [format|2022 13 (@t%n@t%n)&%n|]

    -- part 1: sum of 1-indexes of strictly ordered tuples
    print (sum [i | (i,(x,y)) <- zip [1::Int ..] input, compareT x y == LT])

    -- part 2: product of 1-indexes of two special values in sorted inputs
    let extra = [L[L[N 2]], L[L[N 6]]]
        sorted = sortBy compareT (extra ++ [z | (x,y) <- input, z <- [x,y]])
    print (product [i | (i,x) <- zip [1::Int ..] sorted, x `elem` extra])

-- | Compare two 'T' values together using a lexicographic order on
-- lists and promoting integer nodes to singleton list nodes as needed.
compareT :: T -> T -> Ordering
compareT (N x ) (N y ) = compare x y
compareT (L xs) (L ys) = compareTs xs ys
compareT (N x ) (L ys) = compareTs [N x] ys
compareT (L xs) (N y ) = compareTs xs [N y]

-- | Lexicographic ordering of lists of 'T' values.
compareTs :: [T] -> [T] -> Ordering
compareTs (x:xs) (y:ys) = compareT x y <> compareTs xs ys
compareTs []     []     = EQ
compareTs []     _      = LT
compareTs _      _      = GT
