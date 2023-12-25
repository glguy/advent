{-# Language QuasiQuotes #-}
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
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap (mkEdges, mkNodes, new, NodeMap)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Maybe (fromJust)
import System.Random (randomRIO)

main :: IO ()
main =
 do input <- [format|2023 25 (%s:( %s)*%n)*|]
    let nodeMap :: NodeMap String
        (ns, nodeMap) = mkNodes new (ordNub [n | (k,vs) <- input, n <- k:vs ])
    let g :: Gr Int ()
        g = mkGraph [(n,1) | (n,_) <- ns]
                    (fromJust $ mkEdges nodeMap
                    [(k,v,())
                        | (k,vs) <- input
                        , v <- vs
                        ])

    let loop =
         do g' <- contract (+) g
            if length (edges g') == 3 then
              print (product [sz | (_, sz) <- labNodes g'])
            else putStrLn "retry" >> loop
    loop

contract :: (a -> a -> a) -> Gr a b -> IO (Gr a b)
contract f g
  | noNodes g <= 2 = pure g
  | otherwise =
   do let es = edges g
          n = length es
      i <- randomRIO (0, n-1)
      let (l,r) = es !! i -- pick a random edge to contract
          g1 = delEdge (r,l) (delEdge (l,r) g)
          nei = lneighbors g1 l ++ lneighbors g1 r
          Just sza = lab g l
          Just szb = lab g r
          g2 = insNode (l, f sza szb) (delNodes [l,r] g1)
      contract f (insEdges [(bb,l,aa) | (aa,bb) <- nei] g2)
