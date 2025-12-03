{-# Language BlockArguments #-}
{-|
Module      : Main
Description : Day 3 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/3>

-}
module Main (main) where

import Advent ( getInputLines )
import Advent.Memo ( memo )
import Data.Char ( digitToInt )

-- | >>> :main
-- 17445
-- 173229689350551
main :: IO ()
main =
    do
    input <- map (map digitToInt) <$> getInputLines 2025 3
    let parts = map solveLine input
    print (sum [p  2 | p <- parts])
    print (sum [p 12 | p <- parts])

solveLine :: [Int] -> Int -> Int
solveLine = foldl step (const 0)
  where
    step prev d = memo \n ->
      if n == 0 then 0
      else max (prev (n-1) * 10 + d) (prev n)
