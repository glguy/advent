{-# Language DataKinds, BlockArguments, PatternSynonyms, ParallelListComp #-}
module Advent.KnotHash (knotHash, tieKnots) where

import Advent (chunks, fromDigits, partialSums)
import Advent.Permutation (mkPermutation, runPermutation, Permutation, rotateLeft, rotateRight)
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
  Natural {- ^ rope length    -} ->
  [Int]   {- ^ knot lengths   -} ->
  [Int]   {- ^ resulting rope -}
tieKnots n lengths =
  withSomeSNat n \sn@SNat ->
  runPermutation id $
  mconcat [ p sn offset len
          | offset <- partialSums (zipWith (+) [0..] lengths)
          | len    <- lengths
          ]

p :: SNat n -> Int -> Int -> Permutation n
p n@SNat offset len =
  rotateLeft offset <>
  mkPermutation (\i -> if i < len then len - 1 - i else i) <> -- reverse first length elements
  rotateRight offset
