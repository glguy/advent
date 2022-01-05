{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/2>

Password validation rules.

-}
module Main (main) where

import Advent (countBy, count)
import Advent.Format (format)

type Input = (Int, Int, Char, String)

-- | Check both password validation rules against the list of passwords.
--
-- >>> :main
-- 600
-- 245
main :: IO ()
main =
  do inp <- [format|2020 2 (%u-%u %c: %s%n)*|]
     print (countBy p1 inp)
     print (countBy p2 inp)

-- | Target character must occur between low and high inclusive bounds.
--
-- >>> p1 (1,3,'a',"abcde")
-- True
--
-- >>> p1 (1,3,'b',"cdefg")
-- False
--
-- >>> p1 (2,9,'c',"ccccccccc")
-- True
p1 :: Input -> Bool
p1 (lo,hi,c,str) = lo <= n && n <= hi
  where n = count c str

-- | Target character must occur at two given, 1-based indexes.
--
-- >>> p2 (1,3,'a',"abcde")
-- True
--
-- >>> p2 (1,3,'b',"cdefg")
-- False
--
-- >>> p2 (2,9,'c',"ccccccccc")
-- False
p2 :: Input -> Bool
p2 (i1,i2,c,str) = check i1 /= check i2
  where check i = (str !! (i-1)) == c
