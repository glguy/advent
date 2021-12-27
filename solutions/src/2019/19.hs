{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/19>

-}
module Main (main) where

import Advent.Format (format)
import Data.List (find)
import Intcode (intcodeToList)

-- | >>> :main
-- 211
-- 8071006
main :: IO ()
main =
  do inp <- [format|19 %d&,%n|]
     let f x y = 1 == head (intcodeToList inp [x,y])
     print $ length [ () | x <- [0..49], y <- [0..49], f x y]
     print $ part2 f 0 100

part2 :: (Int -> Int -> Bool) -> Int -> Int -> Int
part2 f x0 y
  | f (x+99) (y-99) = x * 10000 + y - 99
  | otherwise       = part2 f x (y+1)
  where
    Just x = find (`f` y) [x0..]
