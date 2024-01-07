{-# Language QuasiQuotes, BlockArguments, LambdaCase, TransformListComp #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/24>

>>> :{
:main +
"1
2
3
4
5
7
8
9
10
11
"
:}
99
44

-}
module Main (main) where

import Advent.Format (format)
import Advent (pickOne)
import Data.List (sortBy, sort, tails)
import Data.Ord (comparing)
import Advent.Queue (Queue)
import qualified Advent.Queue as Queue

-- order specifically chosen to get desired Ord instance
data Packages = Packages { pkgProduct, pkgSum :: !Int }
  deriving (Eq, Ord, Show)

noPackages :: Packages
noPackages = Packages
  { pkgSum     = 0
  , pkgProduct = 1
  }

addPackage :: Int -> Packages -> Packages
addPackage p pkgs = Packages
  { pkgSum     = pkgSum   pkgs   + p
  , pkgProduct = pkgProduct pkgs * fromIntegral p
  }

solve :: Int -> [Int] -> Int
solve n input = go (Queue.singleton (noPackages, sort input))
  where
    target = sum input `quot` n

    go = \case
      Queue.Empty -> error "no solution"
      (pkg, pkgs) Queue.:<| q
        | target == pkgSum pkg -> pkgProduct pkg
        | otherwise -> go (Queue.appendList q more)
        where
          more = [(pkg', xs) | x:xs <- tails pkgs, let pkg' = addPackage x pkg, then takeWhile by pkgSum pkg' <= target]

-- | Parse the input and print the solutions to both parts.
--
-- >>> :main
-- 11846773891
-- 80393059
main :: IO ()
main =
 do input <- [format|2015 24 (%u%n)*|]
    print (solve 3 input)
    print (solve 4 input)
