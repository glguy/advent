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

I've implemented the Karger-Stein algorithm to produce an infinite
list of candidate min-cuts. This list can then be searched for the
one that has size 3 as required by the problem statement. This allows
the algorithm to terminate as soon as the target cut is found
without having to back that early exit into the algorithm itself.

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
import Advent.MinCut (minCutApprox)
import Advent.Tokenize (autoTokenize)
import Data.Graph.Inductive (UGr, size, labNodes, nmap, mkUGraph)
import System.Random (newStdGen)
import Data.Semigroup (Sum(Sum))

main :: IO ()
main =
 do input <- [format|2023 25 (%s:( %s)*%n)*|]
    let g = nmap (const 1) (simpleGraph (autoTokenize input))
    gs <- minCutApprox g <$> newStdGen
    print (product [sz :: Int | g' <- gs, 3 == size g', then take 1, (_, Sum sz) <- labNodes g'])

-- Transform the input format into an fgl unlabeled graph
simpleGraph :: [(Int, [Int])] -> UGr
simpleGraph input =
  mkUGraph
    (ordNub [n | (k, vs) <- input, n <- k : vs])
    [(k, v) | (k, vs) <- input, v <- vs]
