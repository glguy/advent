{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 1 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/1>

-}
module Main (main) where

import Advent (count, format, stageTH)

data D = DL | DR deriving Show

stageTH

main :: IO ()
main =
 do input <- [format|2025 1 (@D%d%n)*|]
    print (count 0 (scanl step1 50 input))
    print (snd (foldl sim2 (50, 0) input))

step1 :: Int -> (D, Int) -> Int
step1 x (DR, n) = (x + n) `mod` 100
step1 x (DL, n) = (x - n) `mod` 100

sim2 :: (Int, Int) -> (D, Int) -> (Int, Int)
sim2 (here, acc) (DR, n) = (here', acc + zeros)
  where (zeros, here') = (here + n) `divMod` 100
sim2 (here, acc) (DL, n) = (sym here', acc + zeros)
  where (zeros, here') = (sym here + n) `divMod` 100

sym :: Int -> Int
sym x = (-x) `mod` 100
