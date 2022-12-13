{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/13>

-}
module Main where

import Control.Applicative (Alternative((<|>)))
import Data.Char (isDigit)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import Text.ParserCombinators.ReadP (ReadP, sepBy, between, char, munch1)

import Advent (format, stageTH)

data T = N Int | L [T] deriving (Eq, Read, Show)

p :: ReadP T
p = L        <$> between (char '[') (char ']') (p `sepBy` (char ',')) <|>
    N . read <$> munch1 isDigit

stageTH

-- |
-- >>> :main
-- 5340
-- 21276
main :: IO ()
main =
 do input <- [format|2022 13 (@p%n@p%n)&%n|]

    -- part 1
    print (sum [i | (i,(x,y)) <- zip [1::Int ..] input, compareT x y == LT])

    -- part 2
    let extra1 = L [L [N 2]]
        extra2 = L [L [N 6]]
        sorted = sortBy compareT (extra1 : extra2 : [z | (x,y) <- input, z <- [x,y]])
    print ((1 + fromJust (elemIndex extra1 sorted))
          *(1 + fromJust (elemIndex extra2 sorted)))

compareT :: T -> T -> Ordering
compareT (N x) (N y) = compare x y
compareT (L (x:xs)) (L (y:ys)) = compareT x y <> compareT (L xs) (L ys)
compareT (L []) (L []) = EQ
compareT (L []) (L _ ) = LT
compareT (L _)  (L []) = GT
compareT (N x) y = compareT (L [N x]) y
compareT y (N x) = compareT y (L [N x])
