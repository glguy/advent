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
    let g = nmap (const (Sum (1::Int))) (simpleGraph (autoTokenize input))
        makeCandidates = fastmincut makeCandidates g
    gs <- makeCandidates <$> newStdGen
    print (product [sz | g' <- gs, 3 == size g', then take 1, (_, Sum sz) <- labNodes g'])

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
fastmincut k g gen
  | n <= 6, (g', gen') <- contract 2 g gen = g' : k gen'
  | otherwise = rec (rec k) gen -- try twice
  where
    n   = order g
    t   = ceiling (1 + fromIntegral n / sqrt 2 :: Double)
    rec kr = uncurry (fastmincut kr) . contract t g

-- Karger's algorithm parameterized by vertex stop count
contract :: (RandomGen g, Semigroup n) => Int -> Gr n e -> g -> (Gr n e, g)
contract t g gen
  | order g > t
  , ((l, r), gen')               <- pick (edges g) gen
  , (Just (li, _, !szl, lo), g1) <- match l g
  , (Just (ri, _, !szr, ro), g2) <- match r g1
  , let adj = [a | a <- li ++ lo, snd a /= r] ++ ri ++ ro
  , let g3 = insert ([], l, szl <> szr, adj) g2
  = contract t g3 gen'

  | otherwise = (g, gen)

-- Selet a random element from a list
pick :: RandomGen g => [a] -> g -> (a, g)
pick xs gen =
  case randomR (0, length xs - 1) gen of
    (i, gen') -> (xs !! i, gen')
