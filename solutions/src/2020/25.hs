{-# Language ImportQualifiedPost, QuasiQuotes, ViewPatterns #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/25>

Brute-forcing this took me about 6 seconds, but using math makes it instant.

-}
module Main (main) where

import Advent.Format (format)
import Data.Foldable (traverse_)
import GHC.TypeNats (KnownNat, SomeNat(SomeNat), someNatVal)
import Math.NumberTheory.Moduli ((^%), Mod, cyclicGroup, discreteLogarithm, getVal, isMultElement, isPrimitiveRoot)
import Numeric.Natural (Natural)

data DHParams = DH Integer Natural -- ^ generator modulus

params :: DHParams
params = DH 7 20201227

-- |
-- >>> :main
-- 181800
main :: IO ()
main =
  do (pub1,pub2) <- [format|25 %lu%n%lu%n|]
     traverse_ print (hack params pub1 pub2)

hack :: DHParams -> Integer -> Integer -> Maybe Integer
hack (DH g (someNatVal -> SomeNat n)) (toMod n -> public1) (toMod n -> public2) =
  do cg      <- cyclicGroup
     subject <- isPrimitiveRoot cg (fromInteger g)
     public' <- isMultElement public1
     pure (getVal (public2 ^% discreteLogarithm cg subject public'))

toMod :: KnownNat m => proxy m -> Integer -> Mod m
toMod _ = fromInteger
