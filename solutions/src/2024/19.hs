{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/19>

-}
module Main where

import Advent (format, countBy)
import Advent.Memo (memo)
import Data.List (stripPrefix)
import Data.Maybe (mapMaybe)

-- | >>> :main
-- 319
-- 692575723305545
main :: IO ()
main =
 do (available, desired) <- [format|2024 19 %s&(, )%n%n(%s%n)*|]
    let possible = memo \x ->
          if null x
            then 1
            else sum (map possible (mapMaybe (`stripPrefix` x ) available))
    print (countBy (\x -> possible x > 0) desired)
    print (sum (map possible desired))
