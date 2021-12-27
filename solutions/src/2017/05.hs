{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main where

import Advent (format)
import Control.Monad.ST (ST, runST)
import Data.Vector.Unboxed qualified as V
import Data.Vector.Unboxed.Mutable qualified as M

main :: IO ()
main =
  do input <- [format|5 (%d%n)*|]
     print (solve part1 input)
     print (solve part2 input)

-- | Update rules
part1, part2 :: Int -> Int
part1 x             = x+1
part2 x | x >= 3    = x-1
        | otherwise = x+1

-- | Compute the number of steps until the program terminates given
-- an update rule.
--
-- >>> solve part1 [0,3,0,1,-3]
-- 5
-- >>> solve part2 [0,3,0,1,-3]
-- 10
solve ::
  (Int -> Int) {- ^ update rule     -} ->
  [Int]        {- ^ initial program -} ->
  Int          {- ^ steps required  -}
solve f xs = runST (loop 0 0 =<< V.thaw (V.fromList xs))
  where
    loop steps i mem
      | i < 0 || i >= M.length mem = pure $! steps
      | otherwise =
          do d <- M.read mem i
             M.write mem i (f d)
             loop (steps+1) (i+d) mem
