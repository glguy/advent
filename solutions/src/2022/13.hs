{-# Language QuasiQuotes, ImportQualifiedPost, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/13>

-}
module Main where

import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)
import Advent.ReadS (pread, between, runP, sepBy)
import Control.Applicative (Alternative((<|>)))

import Advent (format)

data T = N Int | L [T] deriving (Eq, Read, Show)

-- |
-- >>> :main
-- 5340
-- 21276
main :: IO ()
main =
 do rawinput <- [format|2022 13 (%s%n%s%n)&%n|]
    let input = [(parseT x, parseT y) | (x,y) <- rawinput]

    -- part 1
    print (sum [i | (i,(x,y)) <- zip [1::Int ..] input, compareT x y == LT])

    -- part 2
    let extra1 = parseT "[[2]]"
        extra2 = parseT "[[6]]"
        sorted = sortBy compareT (extra1 : extra2 : [z | (x,y) <- input, z <- [x,y]])
    print ((1 + fromJust (elemIndex extra1 sorted))
          *(1 + fromJust (elemIndex extra2 sorted)))

parseT :: String -> T
parseT = runP go
  where
    go = L <$> between "[" "]" (go `sepBy` ",")
     <|> N <$> pread

compareT :: T -> T -> Ordering
compareT (N x) (N y) = compare x y
compareT (L (x:xs)) (L (y:ys)) = compareT x y <> compareT (L xs) (L ys)
compareT (L []) (L []) = EQ
compareT (L []) (L _ ) = LT
compareT (L _)  (L []) = GT
compareT (N x) y = compareT (L [N x]) y
compareT y (N x) = compareT y (L [N x])
