{-# Language QuasiQuotes, BangPatterns, TransformListComp, BlockArguments #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/25>

<https://en.wikipedia.org/wiki/Karger%27s_algorithm>

I turn the input into a graph annotated with the number of
input nodes each node represents. As nodes are merged by
Karger's algorithm, I update the node labels to account for
the number of nodes each new node represents. This allows
the final answer to be directly computed from the output
of the algorithm.

I've specialized the Karger-Stein algorithm to terminate
early when the target edge count is found.

>>> :{
:main +
"jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
"
:}
54

-}
module Main (main) where

import Advent (format, ordNub)
import Advent.Tokenize (autoTokenize)
import Data.Graph.Inductive (Gr, UGr, insert, match, size, labNodes, edges, nmap, mkUGraph, order)
import System.Random (newStdGen, randomR, RandomGen)
import Data.Semigroup (Sum(Sum))

main :: IO ()
main =
 do input <- [format|2023 25 (%s:( %s)*%n)*|]
    let g = nmap (const 1) (simpleGraph (autoTokenize input))
        makeCandidates = fastmincut makeCandidates g
    gs <- makeCandidates <$> newStdGen
    print (product [sz :: Int | g' <- gs, 3 == size g', then take 1, (_, Sum sz) <- labNodes g'])

-- Transform the input format into an fgl unlabeled graph
simpleGraph :: [(Int, [Int])] -> UGr
simpleGraph input =
  mkUGraph
    (ordNub [n | (k, vs) <- input, n <- k : vs])
    [(k, v) | (k, vs) <- input, v <- vs]

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
