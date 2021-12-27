{-# Language BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/20>

-}
module Main (main) where

import Advent             (getInputArray)
import Advent.Coord       (Coord(C), boundingBox, cardinal, coordLines, above, below, left, right)
import Advent.Search      (AStep(..), astar)
import Data.Array.Unboxed (UArray, listArray, assocs, (!))
import Data.Char          (isAlpha)
import Data.Foldable      (toList)
import Data.Map           (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set

-- | >>> :main
-- 490
-- 5648
main :: IO ()
main =
  do world <- getInputArray 20

     let labels = findLabels world
         links  = findLinks labels
         jumps  = shortcuts world (concat labels)

         Just [start] = Map.lookup "AA" labels
         Just [end  ] = Map.lookup "ZZ" labels

         outside = mkIsOutside labels

         layerChange p
           | outside p = -1
           | otherwise = 1

     print (search (const 0)   jumps links start end)
     print (search layerChange jumps links start end)

-- | Build predicate for coordinates on outer wall.
mkIsOutside ::
  Map String [Coord] {- ^ labeled coordinates -} ->
  Coord -> Bool
mkIsOutside labels = \(C y x) -> x == xhi || x == xlo || y == yhi || y == ylo
  where
    Just (C ylo xlo, C yhi xhi) = boundingBox (concat labels)

data Pos = Pos !Coord !Int
 deriving (Eq, Ord, Show)

search ::
  (Coord -> Int)           {- ^ layer change   -} ->
  Map Coord [(Coord, Int)] {- ^ maze movements -} ->
  Map Coord Coord          {- ^ warp links     -} ->
  Coord                    {- ^ start position -} ->
  Coord                    {- ^ end position   -} ->
  Int                      {- ^ steps to end   -}
search delta jumps links start end =
  snd $ head $ filter isDone $ astar step (Pos start 0)
  where
   isDone (p,_) = Pos end 0 == p

   step (Pos here depth) =
     -- travel through a warp tile
     [ AStep (Pos exit depth') (cost + 1) 0
        | (enter, cost) <- Map.findWithDefault [] here jumps
        , exit          <- toList (Map.lookup enter links)
        , let depth' = depth + delta enter
        , depth' >= 0
        ] ++
     -- finish maze
     [ AStep (Pos enter 0) cost 0
          | depth == 0
          , (enter, cost) <- Map.findWithDefault [] here jumps
          , enter == end
        ]

-- | Find output destinations for each warp tile.
findLinks ::
  Map String [Coord] {- ^ labeled tiles -} ->
  Map Coord Coord    {- ^ warp links    -}
findLinks xs =
  Map.fromList
    do [p1,p2] <- Map.elems xs
       [(p1,p2), (p2,p1)]

-- | Find labeled coordinates.
findLabels :: UArray Coord Char -> Map String [Coord]
findLabels m =
  Map.fromListWith (++)
    [ (lbl, [pos])
    | (pos, '.') <- assocs m
    , (f1, f2)   <- adjFuns
    , let lbl = [m ! f1 pos, m ! f2 pos]
    , all isAlpha lbl
    ]
  where
    adjFuns = [ (left.left, left)
              , (right, right.right)
              , (above.above, above)
              , (below, below.below) ]



-- | Given a list of starting positions find map of destinations
-- and costs to those destinations.
shortcuts :: UArray Coord Char -> [Coord] -> Map Coord [(Coord,Int)]
shortcuts world targets = Map.fromList [(start, travelFrom start) | start <- targets]
  where
    targetSet = Set.fromList targets

    travelFrom src =
      [ (dst,n)
         | (dst,n) <- astar step src
         , Set.member dst targetSet
         , dst /= src
         ]

    step here = [AStep there 1 0 | there <- cardinal here, world ! there == '.']
