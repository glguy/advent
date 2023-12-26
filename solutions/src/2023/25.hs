{-# Language QuasiQuotes, BangPatterns #-}
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
import Data.Graph.Inductive (Gr, UGr, insert, match, size, labNodes, edges, nmap, mkUGraph, noNodes)
import System.Random (getStdRandom, randomR, RandomGen, StdGen)

main :: IO ()
main =
 do input <- [format|2023 25 (%s:( %s)*%n)*|]
    let g = nmap (const 1) (simpleGraph (autoTokenize input))
        loop gen =
          case fastmincut g gen of
            (g', gen')
              | size g' == 3 -> (g', gen')
              | otherwise    -> loop gen'
    g' <- getStdRandom loop
    print (product [sz | (_, sz) <- labNodes g'])

-- Transform the input format into an fgl unlabeled graph
simpleGraph :: [(Int, [Int])] -> UGr
simpleGraph input =
  mkUGraph
    (ordNub [n | (k,vs) <- input, n <- k:vs])
    [(k,v) | (k,vs) <- input, v <- vs]

-- Karger–Stein algorithm (specialized to find mincuts of size 3)
fastmincut :: Gr Int e -> StdGen -> (Gr Int e, StdGen)
fastmincut g gen
  | n <= 6    = contract 2 g gen
  | otherwise =
      case rec gen of
        (g', gen') | size g' == 3 -> (g', gen')
                   | otherwise    -> rec gen'
  where
    n   = noNodes g
    t   = ceiling (1 + fromIntegral n / sqrt 2 :: Double)
    rec = uncurry fastmincut . contract t g

-- Karger algorithm parameterized by stop condition
contract :: RandomGen g => Int -> Gr Int e -> g -> (Gr Int e, g)
contract t g gen
  | noNodes g > t
  , ((l, r), gen')               <- pick (edges g) gen
  , (Just (li, _, !szl, lo), g1) <- match l g
  , (Just (ri, _, !szr, ro), g2) <- match r g1
  , let adj = [a | a <- li ++ lo, snd a /= r] ++ ri ++ ro
  , let g3 = insert ([], l, szl + szr, adj) g2
  = contract t g3 gen'

  | otherwise = (g, gen)

-- Selet a random element from a list
pick :: RandomGen g => [a] -> g -> (a, g)
pick xs gen =
  case randomR (0, length xs - 1) gen of
    (i, gen') -> (xs !! i, gen')
