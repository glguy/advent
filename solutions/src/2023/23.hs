{-# Language LambdaCase, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Main
Description : Day 23 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/23>

A brute-forced approach.

-}
module Main (main) where

import Advent (getInputArray, arrIx)
import Advent.Coord (cardinal, coordRow, east, north, south, west, Coord(C))
import Advent.Search (bfs, dfs)
import Data.Array.Unboxed (bounds, UArray)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set

main :: IO ()
main =
 do input <- getInputArray 2023 23
    let (_, C ymax _) = bounds input
    let path = [length s | (C y _,s) <- dfs (step input) (C 0 1, Set.empty), y == ymax]
    print (maximum path)

    let input' = buildPaths input
    let solvable here seen =
          any (\x -> coordRow x == ymax) $
          bfs (\c -> [next | (next, _) <- Map.findWithDefault [] c input'
                            , Set.notMember next seen]) here

    let search _ [] = []
        search best ((C y _, dist, _):xs) | y == ymax = dist : search best xs
        search best ((here, dist, seen):xs)
          | Map.findWithDefault (-1) (here, seen) best < dist =
          search (Map.insert (here,seen) dist best) (
          [(next, dist+dist1, Set.insert here seen)
            | (next, dist1) <- sortBy (flip (comparing snd))
                 (Map.findWithDefault [] here input')
            , Set.notMember next seen
            , solvable here seen
          ] ++ xs)
          | otherwise = search best xs

    print (maximum (search Map.empty [(C 0 1, 0, Set.empty)]))

buildPaths :: UArray Coord Char -> Map Coord [(Coord, Int)]
buildPaths input = go [C 0 1] Map.empty
  where
    isIntersection c =
      2 < length [
        c'
        | c' <- cardinal c
        , cell <- arrIx input c'
        , isOpen cell
      ] || coordRow c == ymax
    (_,C ymax _) = bounds input

    go [] acc = acc
    go (x:xs) acc
      | Map.member x acc = go xs acc
      | otherwise = go (map fst ends ++ xs) (Map.insert x ends acc)
      where
        ends =
          map (\(p:path) -> (p, length path)) $
          filter (isIntersection . head) $
          bfs next [x]

        next (c:_) | c /= x, isIntersection c = []
        next xxs@(c:cs) =
          [ c' : xxs
            | c' <- cardinal c
            , c' `notElem` cs
            , cell <- arrIx input c'
            , isOpen cell
          ]
        next [] = undefined

step :: UArray Coord Char -> (Coord, Set Coord) -> [(Coord, Set Coord) ]
step input (c, seen) =
  [ (c', Set.insert c seen)
     | c' <- cardinal c
     , cell <- arrIx input c'
     , case cell of
         '.' -> True
         '>' -> c' - c == east
         'v' -> c' - c == south
         '^' -> c' - c == north
         '<' -> c' - c == west
         _ -> False
     , Set.notMember c' seen ]

isOpen :: Char -> Bool
isOpen = \case
  '.' -> True
  '>' -> True
  'v' -> True
  '^' -> True
  '<' -> True
  _   -> False
