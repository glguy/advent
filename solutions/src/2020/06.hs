{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 6 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/6>

-}
module Main (main) where

import Advent.Format (format)
import Data.List (intersect, union)

-- |
-- >>> :main
-- 6273
-- 3254
main :: IO ()
main =
  do inp <- [format|2020 6 (%s%n)*&%n|]
     print (length (foldr union []   =<< inp))
     print (length (foldl1 intersect =<< inp))
