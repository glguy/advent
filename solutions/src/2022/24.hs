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
import Data.Array.Unboxed (UArray, (!), bounds, inRange)

import Advent (getInputArray)
import Advent.Coord (Coord(..), cardinal, west)

data SearchState = S {
  time :: !Int,
  world :: !(Set Coord)
}

-- |
-- >>> :main
-- 295
-- 851
main :: IO ()
main =
 do input <- getInputArray 2022 24
    let start = C 0 1
        end   = snd (bounds input) + west
        t1    = shortest input 0  start end
        t2    = shortest input t1 end   start
        t3    = shortest input t2 start end
    print t1
    print t3

-- | Find the shortest time from source to destination starting at a
-- particular time step.
shortest ::
  UArray Coord Char {- ^ input map           -} ->
  Int               {- ^ initial time step   -} ->
  Coord             {- ^ starting location   -} ->
  Coord             {- ^ ending location     -} ->
  Int               {- ^ minimum travel time -}
shortest input t src dst =
    time (until (Set.member dst . world) (step input) (S t (Set.singleton src)))

-- | Given a set of locations the elf could be find the set the elf can be at next.
step ::
  UArray Coord Char {- ^ input map -} ->
  SearchState -> SearchState
step w (S t prev) =
  S (t+1) $
  Set.fromList
    [ next
      | here <- Set.toList prev
      , next <- here : cardinal here
      , isOpen w (t+1) next
    ]

-- | Check if a location in the world is available
isOpen ::
  UArray Coord Char {- ^ input map          -} ->
  Int               {- ^ time step          -} ->
  Coord             {- ^ location           -} ->
  Bool              {- ^ location available -}
isOpen w t here@(C y x) =
  inRange (bounds w) here            &&
  w ! here                    /= '#' && -- walls
  w ! (C y ((x'-t)`mod`xm+1)) /= '>' && -- east-bound storms
  w ! (C y ((x'+t)`mod`xm+1)) /= '<' && -- west-bound storms
  w ! (C ((y'-t)`mod`ym+1) x) /= 'v' && -- south-bound storms
  w ! (C ((y'+t)`mod`ym+1) x) /= '^'    -- north-bound storms
  where
    y' = y-1
    x' = x-1
    C ym xm = snd (bounds w) - 1
