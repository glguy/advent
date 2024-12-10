{-# Language DataKinds, BlockArguments, PatternSynonyms #-}
module Advent.KnotHash (knotHash, tieKnots) where

import Advent (chunks, fromDigits, partialSums)
import Advent.Permutation (mkPermutation, runPermutation, Permutation)
import Data.Bits (Bits(xor))
import Data.Char (ord)
import Data.Foldable (Foldable(foldl'))
import Data.List (foldl1')
import Data.Word (Word8)
import GHC.TypeNats (SNat, pattern SNat, natSing, natVal, withSomeSNat)
import Numeric.Natural (Natural)

-- | Given a rope size and an input string, compute the resulting hash.
knotHash ::
  String  {- ^ input string -} ->
  Integer {- ^ knot value   -}
knotHash =
   fromDigits 256 . map (fromIntegral . foldl1' xor) .
   chunks 16 . tieKnots 256 . concat . replicate 64 .
   (++ [17, 31, 73, 47, 23]) . map ord

-- | Create a rope, tie knots of the given lengths while skipping
-- according to the increasing skip rule.
tieKnots ::
  Natural ->
  [Int]   {- ^ knot lengths   -} ->
  [Word8] {- ^ resulting rope -}
tieKnots n lengths =
  withSomeSNat n \sn@SNat ->
  runPermutation fromIntegral $
  mconcat [ p sn o l
          | (o,l) <- zip (partialSums (zipWith (+) [0..] lengths)) lengths
          ]

p :: SNat n -> Int -> Int -> Permutation n
p n@SNat o l =
  mkPermutation \i ->
  if (i-o)`mod` fromIntegral (natVal n) < l
  then l-1-i+o+o
  else i
