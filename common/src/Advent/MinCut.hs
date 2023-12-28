{-# Language BangPatterns #-}
{-|
Module      : Advent.MinCut
Description : Minimum cut graph algorithm
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

Karger-Stein approximation of the minimum cut of a graph.

-}
module Advent.MinCut (minCutApprox) where

import Data.Graph.Inductive (match, Gr, insert, edges, order)
import System.Random (StdGen, RandomGen, randomR)

-- | Generate a lazy list of minimun cut approximations.
-- The nodes of the resulting graph will represent the
-- merged components and the remaining edges are the cut.
--
-- The graph is treated as undirected and with uniform edge weight.
minCutApprox ::
  RandomGen gen => Semigroup node =>
  Gr node edge -> gen -> [Gr node edge]
minCutApprox gr = fastmincut (minCutApprox gr) gr
{-# SPECIALIZE minCutApprox :: Semigroup node => Gr node edge -> StdGen -> [Gr node edge] #-}

-- Karger–Stein algorithm parameterized over the continuation
-- that consumes the random generator. This allows the implementation
-- to generate an infinite list of candidates to be selected from.
-- The 'Semigroup' instance is used to combine merged nodes.
fastmincut ::
  RandomGen gen => Semigroup node =>
  (gen -> [Gr node edge]) {- ^ continuation -} ->
  Gr node edge -> gen -> [Gr node edge]
fastmincut k gr gen
  | n <= 6, (gr', gen') <- contract 2 gr gen = gr' : k gen'
  | otherwise = rec (rec k) gen -- try twice
  where
    n   = order gr
    t   = ceiling (1 + fromIntegral n / sqrt 2 :: Double)
    rec k' gen1 | (gr', gen2) <- contract t gr gen1 = fastmincut k' gr' gen2

-- Karger's algorithm parameterized by vertex stop count
contract ::
  RandomGen gen => Semigroup node =>
  Int -> Gr node edge -> gen -> (Gr node edge, gen)
contract t gr gen
  | order gr > t
  , ((l, r), gen1)                <- pick (edges gr) gen
  , (Just (li, _, !szl, lo), gr1) <- match l gr
  , (Just (ri, _, !szr, ro), gr2) <- match r gr1
  , let adj = [a | a <- li ++ lo, snd a /= r] ++ ri ++ ro
  , let gr3 = insert ([], l, szl <> szr, adj) gr2
  = contract t gr3 gen1

  | otherwise = (gr, gen)

-- Selet a random element from a list
pick :: RandomGen gen => [a] -> gen -> (a, gen)
pick xs gen =
  case randomR (0, length xs - 1) gen of
    (i, gen') -> (xs !! i, gen')
