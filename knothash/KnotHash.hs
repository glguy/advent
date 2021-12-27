{-# Language ImportQualifiedPost, DataKinds #-}
module KnotHash (knotHash, tieKnots) where

import Advent (chunks)
import Advent.Permutation (mkPermutation, runPermutation, Permutation)
import Data.Bits (Bits(xor))
import Data.Char (ord)
import Data.Foldable (Foldable(foldl'))
import Data.List (foldl1')
import Data.Word (Word8)

-- | Given a rope size and an input string, compute the resulting hash.
knotHash ::
  String  {- ^ input string -} ->
  Integer {- ^ knot value   -}
knotHash =
   bytesToInteger . map (foldl1' xor) .
   chunks 16 . tieKnots . concat . replicate 64 .
   (++ [17, 31, 73, 47, 23]) .  map ord

-- | Convert list of bytes into integer in big-endian order.
bytesToInteger :: [Word8] -> Integer
bytesToInteger = foldl' (\acc x -> acc * 0x100 + fromIntegral x) 0

-- | Create a rope, tie knots of the given lengths while skipping
-- according to the increasing skip rule.
tieKnots ::
  [Int]   {- ^ knot lengths   -} ->
  [Word8] {- ^ resulting rope -}
tieKnots lengths = runPermutation fromIntegral
                 $ mconcat [ p o l
                           | (o,l) <- zip (scanl (+) 0 (zipWith (+) [0..] lengths)) lengths
                           ]

p :: Int -> Int -> Permutation 256
p o l = mkPermutation $ \i -> if (i-o)`mod`256 < l
                                   then l-1-i+o+o
                                   else i
