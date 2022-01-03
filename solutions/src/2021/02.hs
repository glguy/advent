{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/2>

Implement a simple submarine piloting/aiming command interpreter.

-}
module Main (main) where

import Advent (format)
import Data.Foldable (foldMap')

-- | Three possible commands a submarine can receive.
data C = Cforward | Cdown | Cup

mempty -- puts C into view of format's reify below

-- | >>> :main
-- 1636725
-- 1872757425
main :: IO ()
main =
 do inp <- [format|2 (@C %u%n)*|]
    case foldMap' toS inp of
      S dx dy1 dy2 ->
       do print (dx*dy1)
          print (dx*dy2)

-- | Computes the individual effect of a single instruction on a submarine.
toS :: (C, Int) -> S
toS (c,n) =
  case c of
    Cup      -> S 0 (-n) 0
    Cdown    -> S 0 n    0
    Cforward -> S n 0    0

-- | Tracks the current state of the submarine's x displacement
-- as well as the displacement for parts 1 and 2
data S = S !Int !Int !Int -- dx dy1 dy2
  deriving Show

-- | A submarine that hasn't moved and is at the origin.
instance Monoid S where mempty = S 0 0 0

-- | Composes two submarine movements from left to right.
instance Semigroup S where S x1 y1 z1 <> S x2 y2 z2 = S (x1+x2) (y1+y2) (z1+z2+y1*x2)
