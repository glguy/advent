{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/25>

-}
module Main (main) where

import Advent.Format ( format )
import Data.Graph.Inductive.Graph (mkUGraph)
import Data.Graph.Inductive.PatriciaTree (UGr)
import Data.Graph.Inductive.Query (noComponents)

-- | Print the answers to day 25
main :: IO ()
main =
 do input <- [format|25 (%d&,%n)*|]
    print (noComponents (starGraph input))

starGraph :: [[Int]] -> UGr
starGraph stars =
  mkUGraph [ 1 .. length stars ]
           [ (i,j) | (i,x) <- zip [1..] stars
                   , (j,y) <- zip [1..] stars
                   , manhattan x y <= 3 ]

manhattan :: [Int] -> [Int] -> Int
manhattan x y = sum (zipWith (\a b -> abs (a-b)) x y)
