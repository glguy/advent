{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/20>

>>> :main + "1\n2\n-3\n3\n-2\n0\n4\n"
3
1623178306

-}
module Main where

import Data.Sequence qualified as Seq

import Advent (format)

-- |
-- >>> :main
-- 1591
-- 14579387544492
main :: IO ()
main = do
    input <- [format|2022 20 (%d%n)*|]
    print (grove 1 input)
    print (grove 10 (map (811589153*) input))

grove :: Int -> [Int] -> Int
grove n xs = sum [Seq.index s ((z+i)`mod`Seq.length s) | i <- [1000,2000,3000]]
  where
    s = run n xs
    Just z = Seq.elemIndexL 0 s

run :: Int -> [Int] -> Seq.Seq Int
run n seed = go (Seq.fromList (zip [1..] seed)) (concat (replicate n [1..length seed]))
  where
    go s [] = snd <$> s
    go s (x:xs) = go (Seq.insertAt d (x,v) (b <> a)) xs
        where
            Just i = Seq.findIndexL (\t -> fst t == x) s
            (a, (_,v) Seq.:<| b) = Seq.splitAt i s
            d = v `mod` (Seq.length s - 1)