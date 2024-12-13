{-# Language QuasiQuotes, TemplateHaskell, BlockArguments, LambdaCase, ImportQualifiedPost #-}
{-# OPTIONS_GHC -w #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/13>

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.List
import Data.Maybe
import Data.Array.Unboxed

import Advent
import Advent.Coord
import Advent.Search
import System.Environment

main :: IO ()
main =
  -- withArgs ["/Users/emertens/Source/advent/example.txt"]
    do
    input <- [format|2024 13 (%s%n)*|]
    print input