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
import Data.Graph.Inductive (Gr, UGr, (&), match, labNodes, edges, nmap, mkUGraph, noNodes)
import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import System.Random (randomRIO)

main :: IO ()
main =
 do input <- [format|2023 25 (%s:( %s)*%n)*|]
    let g = nmap (const 1) (simpleGraph (autoTokenize input))
        loop (x:xs) =
         do g' <- contract (+) g
            if length (edges g') == 3 then
                print (product [sz | (_, sz) <- labNodes g'])
            else
             do putChar x
                putChar '\^H'
                hFlush stdout
                loop xs
    loop (cycle "←↖↑→↘↓↙")

simpleGraph :: [(Int, [Int])] -> UGr
simpleGraph input =
  mkUGraph
    (ordNub [n | (k,vs) <- input, n <- k:vs])
    [(k,v) | (k,vs) <- input, v <- vs]

contract :: (a -> a -> a) -> Gr a b -> IO (Gr a b)
contract combineNodeLabels g
  | noNodes g <= 2 = pure g
  | otherwise =
   do let es = edges g
      i <- randomRIO (0, length es - 1)
      let (l,r) = es !! i -- pick a random edge to contract
          (Just (li, _, !szl, lo), g1) = match l g
          (Just (ri, _, !szr, ro), g2) = match r g1
          adj = [a | a@(_,n) <- li ++ lo, n /= r]
             ++ [a | a@(_,n) <- ri ++ ro]
          g3 = ([], l, combineNodeLabels szl szr, adj) & g2
      contract combineNodeLabels g3
