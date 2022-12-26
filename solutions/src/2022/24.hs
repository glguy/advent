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

-- |
-- >>> :main
-- 295
-- 851
main :: IO ()
main =
 do input <- getInputArray 2022 24
    let start = C 0 1
    let end   = snd (bounds input) + west

    let loop t end prev
          | Set.member end prev = t
          | otherwise = loop (t+1) end (grow input (t+1) prev)

    let t1 = loop 0  end   (Set.singleton start)
        t2 = loop t1 start (Set.singleton end  )
        t3 = loop t2 end   (Set.singleton start)
    print t1
    print t3

-- | Given a set of locations the elf could be find the set the elf can be at next.
grow ::
  UArray Coord Char {- ^ input map          -} ->
  Int               {- ^ time step          -} ->
  Set Coord         {- ^ previous locations -} ->
  Set Coord         {- ^ next locations     -}
grow w t prev =
  Set.fromList
    [ next
      | here <- Set.toList prev
      , next <- here : cardinal here
      , inRange (bounds w) next
      , isOpen w t next
    ]

-- | Check if a location in the world is available
isOpen ::
  UArray Coord Char {- ^ input map          -} ->
  Int               {- ^ time step          -} ->
  Coord             {- ^ location           -} ->
  Bool              {- ^ location available -}
isOpen w t here@(C y x) =
  w ! here                    /= '#' &&
  w ! (C y ((x'-t)`mod`xm+1)) /= '>' &&
  w ! (C y ((x'+t)`mod`xm+1)) /= '<' &&
  w ! (C ((y'-t)`mod`ym+1) x) /= 'v' &&
  w ! (C ((y'+t)`mod`ym+1) x) /= '^'
  where
    y' = y-1
    x' = x-1
    C ym xm = snd (bounds w) - 1
