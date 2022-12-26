{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/24>

>>> :{
:main +
  "#.######\n\
  \#>>.<^<#\n\
  \#.<..<<#\n\
  \#>v.><>#\n\
  \#<^v^^>#\n\
  \######.#\n"
:}
18
54

-}
module Main where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Array.Unboxed

import Advent ( getInputArray )
import Advent.Coord (cardinal, west, Coord(..))

checkCell :: UArray Coord Char -> Int -> Coord -> Bool
checkCell w t here@(C y x) =
  w!here /= '#' &&
  w!(C y ((x'-t)`mod`xm+1)) /= '>' &&
  w!(C y ((x'+t)`mod`xm+1)) /= '<' &&
  w!(C ((y'-t)`mod`ym+1) x) /= 'v' &&
  w!(C ((y'+t)`mod`ym+1) x) /= '^'
  where
    y' = y-1
    x' = x-1
    C ym xm = snd (bounds w) - 1

-- |
-- >>> :main
-- 295
-- 851
main :: IO ()
main =
 do input <- getInputArray 2022 24
    let (_,hi) = bounds input
    let target = hi + west

    let loop t end prev
          | Set.member end prev = t
          | otherwise = loop (t+1) end (grow input (t+1) prev)

    let t1 = loop 0  target  (Set.singleton (C 0 1))
        t2 = loop t1 (C 0 1) (Set.singleton target)
        t3 = loop t2 target  (Set.singleton (C 0 1))
    print t1
    print t3

-- | Given a set of locations the elf could be find the set the elf can be at next.
grow :: UArray Coord Char -> Int -> Set Coord -> Set Coord
grow w t prev =
  Set.fromList
    [ next
      | here <- Set.toList prev
      , next <- here : cardinal here
      , inRange (bounds w) next
      , checkCell w t next
    ]
