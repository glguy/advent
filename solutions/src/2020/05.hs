{-# Language TemplateHaskell, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/5>

-}
module Main (main) where

import Advent.Format (format)
import Data.List (sort)

data H = HL | HR | HF | HB
pure[]

-- |
-- >>> :main
-- 951
-- 653
main :: IO ()
main =
  do inp <- [format|5 (@H*%n)*|]
     let seatIds = map seatId inp
     print (maximum seatIds)
     print (gap (sort seatIds))

gap :: [Int] -> Int
gap (x:y:_) | x+2 == y = x+1
gap (_:xs) = gap xs
gap [] = error "couldn't find a gap"

seatId :: [H] -> Int
seatId xs = let (r,c) = seat xs in 8*r+c

seat :: [H] -> (Int,Int)
seat = foldl f (0,0)
  where
    f (r,c) HL = (r,2*c  )
    f (r,c) HR = (r,2*c+1)
    f (r,c) HF = (2*r  ,c)
    f (r,c) HB = (2*r+1,c)
