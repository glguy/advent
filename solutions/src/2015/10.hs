{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2015
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/10>

-}
module Main where

import Advent.Input (getInputLines)
import Data.List (group)

-- |
-- >>> :main
-- 252594
-- 3579328
main :: IO ()
main =
  do [start] <- getInputLines 2015 10
     let steps = iterate lookAndSay start
     print (length (steps !! 40))
     print (length (steps !! 50))

-- | Look and say process.
--
-- >>> lookAndSay "1"
-- "11"
--
-- >>> lookAndSay "11"
-- "21"
--
-- >>> lookAndSay "21"
-- "1211"
--
-- >>> lookAndSay "1211"
-- "111221"
--
-- >>> lookAndSay "111221"
-- "312211"
lookAndSay :: String -> String
lookAndSay = foldr aux [] . group
  where
  aux xs = shows (length xs)
         . showChar (head xs)
