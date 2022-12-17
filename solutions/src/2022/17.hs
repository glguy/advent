{-# Language QuasiQuotes, NumericUnderscores, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/17>

This solution looks for cycles when the move index, piece index,
and tower envelope repeat. The tower envelope is the set of rocks that are
reachable from the row above the top of the tower.

>>> :main + ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>\n"
3068
1514285714288

-}
module Main where

import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Array

import Advent (format)
import Advent.Coord (Coord(C), coordRow, east, west, cardinal, coordCol, south)
import Advent.Search (bfsN)

-- | The set of five blocks
--
-- >>> mapM_ (putStrLn . Advent.Coord.drawCoords) pieces
-- ████
-- <BLANKLINE>
-- ·█·
-- ███
-- ·█·
-- <BLANKLINE>
-- ··█
-- ··█
-- ███
-- <BLANKLINE>
-- █
-- █
-- █
-- █
-- <BLANKLINE>
-- ██
-- ██
-- <BLANKLINE>
pieces :: Array Int (Set Coord)
pieces = listArray (0,4) [
    Set.fromList [C 0 0, C 0 1, C 0 2, C 0 3],
    Set.fromList [C (-2) 1, C (-1) 0, C (-1) 1, C (-1) 2, C 0 1],
    Set.fromList [C 0 0, C 0 1, C 0 2, C (-1) 2, C (-2) 2],
    Set.fromList [C 0 0, C (-1) 0, C (-2) 0, C (-3) 0],
    Set.fromList [C (-1) 0, C (-1) 1, C 0 0, C 0 1]
  ]

-- | The initial floor
initialStuff :: Set Coord
initialStuff = Set.fromList [C 0 x | x <- [0..6]]

-- |
-- >>> :main
-- 3137
-- 1564705882327
main :: IO ()
main =
 do moves <- map dir <$> [format|2022 17 %s%n|]
    let movesArray = listArray (0, length moves-1) moves

    let states = iterate (place movesArray) (0, 0, initialStuff)
    let heightAt i = case states !! i of (_,_,stuff) -> height stuff

    -- part 1
    print (heightAt 2022)

    -- part 2
    let (cyc1,cyc2) = findCycle [(i,j,normalize stuff) | (i,j,stuff) <- states]
    let cycLen = cyc2 - cyc1

    let (cycCnt, overflow) = (1_000_000_000_000 - cyc1) `divMod` cycLen
    let cycHeight = heightAt cyc2 - heightAt cyc1
    print (heightAt (cyc2 + overflow) + cycHeight * (cycCnt-1))

-- | Height of a tower
height :: Set Coord -> Int
height stuff = - coordRow (minimum stuff)

-- | Renumber a tower so that it's top starts at 0
normalize :: Set Coord -> Set Coord
normalize stuff = translate stuff (C (height stuff) 0)

-- | Returns two indexes showing where a cycle starts and ends
findCycle :: Ord a => [a] -> (Int,Int)
findCycle = go Map.empty 0
  where
    go _ _ [] = error "no cycle"
    go seen i (x:xs)
       | Just j <- Map.lookup x seen = (j,i)
       | otherwise = go (Map.insert x i seen)(i+1) xs

-- | Map the input characters to jet vectors
dir :: Char -> Coord
dir '>' = east
dir '<' = west
dir  _  = error "bad dir"

-- | Predicate for coordinates that are inside the tower walls
inWalls :: Coord -> Bool
inWalls (C _ x) = 0 <= x && x <= 6

-- | Translate a piece around by a vector
translate :: Set Coord -> Coord -> Set Coord
translate p c = Set.mapMonotonic (c+) p

-- | Remove all blocks that aren't reachable from the top of the board
clean :: Set Coord -> Set Coord
clean stuff = Set.filter alive stuff
  where
    ymin = coordRow (minimum stuff)
    step c = [n | n <- cardinal c, 0 <= coordCol n, coordCol n <= 6, coordRow c >= ymin, Set.notMember n stuff]
    air = bfsN step [C ymin x | x <- [0..6], Set.notMember (C ymin x) stuff]
    alive x = any (`elem` air) (cardinal x) || coordRow x == ymin

place ::
  Array Int Coord {- ^ jet vectors -} ->
  (Int, Int, Set Coord) {- ^ piece index, jet index, rocks -} ->
  (Int, Int, Set Coord)
place jets (i,j,stuff) =
    case drive j piece' of
      (stuck, j') -> (i', j', clean (Set.union stuff stuck))
    where
      i' = (i+1)`mod`5
      p  = pieces ! i
      piece' = translate p (C (-height stuff-4) 2)

      isCrashed piece = not (all inWalls piece) || not (Set.disjoint stuff piece)

      drive dj p1
        | isCrashed p4 = (p3, dj')
        | otherwise = drive dj' p4
        where
            dj' = (dj+1) `mod` length jets
            x  = jets ! dj
            p2 = translate p1 x
            p3 | isCrashed p2 = p1
               | otherwise    = p2
            p4 = translate p3 south
