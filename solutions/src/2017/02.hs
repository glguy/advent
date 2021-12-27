{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main where

import Advent (format)
import Data.List (delete)

main :: IO ()
main =
  do xs <- [format|2 (%u&%t%n)*|]
     print (sum (map checksum1 xs))
     print (sum (map checksum2 xs))

-- | First checksum is the difference of the largest and smallest elements
--
-- >>> checksum1 [5,1,9,5]
-- 8
-- >>> checksum1 [7,5,3]
-- 4
-- >>> checksum1 [2,4,6,8]
-- 6
checksum1 :: [Int] -> Int
checksum1 xs = maximum xs - minimum xs

-- | Second checksum is the quotient of the only two elements that evenly
-- divide each other.
--
-- >>> checksum2 [5,9,2,8]
-- 4
-- >>> checksum2 [9,4,7,3]
-- 3
-- >>> checksum2 [3,8,6,5]
-- 2
checksum2 :: [Int] -> Int
checksum2 xs =
  head [ q | x <- xs, y <- delete x xs, (q,0) <- [x `divMod` y] ]
