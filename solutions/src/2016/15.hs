{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/15>

-}
module Main where

data Disc = Disc !Int !Int deriving Show

main :: IO ()
main =
  do print (solve input1)
     print (solve input2)

input1, input2 :: [Disc]
input1 =
  [ Disc 13  1
  , Disc 19 10
  , Disc  3  2
  , Disc  7  1
  , Disc  5  3
  , Disc 17  5
  ]
input2 = input1 ++ [Disc 11 0]

-- | Correct a disc for when the object will reach it
fixup :: Int -> Disc -> Disc
fixup i (Disc a b) = Disc a (i+b)

-- | Figure out what time to drop the capsule
solve :: [Disc] -> Int
solve = snd
      . foldl aux (1,0)
      . zipWith fixup [1..]

aux :: (Int,Int) -> Disc -> (Int,Int)
aux (stepSize, wait) (Disc a b) = (lcm stepSize a, wait')
  where
    wait':_= filter (\i -> (i+b)`rem`a == 0) [wait, wait+stepSize .. ]
