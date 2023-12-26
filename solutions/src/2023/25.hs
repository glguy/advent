{-# Language QuasiQuotes, BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/25>

<https://en.wikipedia.org/wiki/Karger%27s_algorithm>

-}
module Main (main) where

import Advent (format, ordNub)
import Advent.Tokenize (autoTokenize)
import Data.Graph.Inductive (Gr, UGr, (&), match, size, labNodes, edges, nmap, mkUGraph, noNodes)
import System.Random (randomRIO)

main :: IO ()
main =
 do input <- [format|2023 25 (%s:( %s)*%n)*|]
    let g = nmap (const 1) (simpleGraph (autoTokenize input))
        loop =
         do g' <- fastmincut g
            if size g' == 3 then pure g' else loop
    g' <- loop
    print (product [sz | (_, sz) <- labNodes g'])

simpleGraph :: [(Int, [Int])] -> UGr
simpleGraph input =
  mkUGraph
    (ordNub [n | (k,vs) <- input, n <- k:vs])
    [(k,v) | (k,vs) <- input, v <- vs]

-- Karger–Stein algorithm (specialized to find mincuts of size 3)
fastmincut :: Gr Int b -> IO (Gr Int b)
fastmincut g
  | n <= 6 = contract 2 g
  | otherwise =
   do let t = ceiling (1 + fromIntegral n / sqrt 2 :: Double)
          attempt = fastmincut =<< contract t g
      g' <- attempt
      if size g' == 3 then pure g' else attempt
  where
    n = noNodes g

-- Karger algorithm parameterized by stop condition
contract :: Int -> Gr Int b -> IO (Gr Int b)
contract t g
  | noNodes g <= t = pure g
  | otherwise =
   do (l, r) <- pick (edges g)
      let (Just (li, _, !szl, lo), g1) = match l g
          (Just (ri, _, !szr, ro), g2) = match r g1
          adj = [a | a@(_,n) <- li ++ lo, n /= r]
             ++ ri ++ ro
          g3 = ([], l, szl + szr, adj) & g2
      contract t g3

-- Selet a random element from a list
pick :: [a] -> IO a
pick xs =
 do i <- randomRIO (0, length xs - 1)
    pure $! xs !! i
