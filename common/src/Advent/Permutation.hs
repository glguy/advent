{-# Language ImportQualifiedPost, RoleAnnotations, DataKinds, KindSignatures #-}
{-|
Module      : Advent.Permutation
Description : Composable permutations
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Common permutations of a finite collection of elements and
operations on them.
-}
module Advent.Permutation
  ( Permutation
  , runPermutation
  , mkPermutation
  , swap
  , rotateRight
  , rotateLeft
  , invert
  , isValid
  , size
  , backwards
  , cycles
  , order
  ) where

import Advent.Group (Group(..))
import Data.Function (fix)
import Data.IntSet qualified as IntSet
import Data.List (foldl', unfoldr)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup
import Data.Vector.Unboxed qualified as V
import GHC.TypeLits

-- $setup
-- >>> :set -XDataKinds

type role Permutation nominal

-- | Permutations of @n@ elements
newtype Permutation (n :: Nat) = P (V.Vector Int)
  deriving (Eq, Ord, Read, Show)

runPermutation :: (Int -> a) -> Permutation n -> [a]
runPermutation f (P v) = f <$> V.toList v

-- | Size of the list of elements permuted.
size :: Permutation n -> Int
size (P v) = V.length v

-- | Validate a permutation. A valid permutation will map each element in the input
-- to a unique element in the output.
isValid :: Permutation n -> Bool
isValid (P p) = V.and (V.accumulate_ (\_ new -> new) (V.replicate n False) p (V.replicate n True))
  where
    n = V.length p

-- | Helper function for making the size of a requested permutation available
-- while building the permutation.
withSize :: KnownNat n => (Int -> Permutation n) -> Permutation n
withSize f = fix (f . fromIntegral . natVal)

-- | Given a function mapping incoming indices to outgoing ones, construct
-- a new permutation value.
mkPermutation :: KnownNat n => (Int -> Int) -> Permutation n
mkPermutation f = withSize $ \n -> P $ V.generate n $ \i -> f i `mod` n

-- | Permutation generated by swapping the elements at a pair of indices.
swap :: KnownNat n => Int -> Int -> Permutation n
swap x y = withSize $ \n ->
  let x' = x`mod`n -- not evaluated when n == 0
      y' = y`mod`n
  in mkPermutation $ \i -> if i == x' then y' else if i == y' then x' else i

-- | Permutation generated by rotating all the elements to the right.
rotateRight :: KnownNat n => Int -> Permutation n
rotateRight = rotateLeft . negate

-- | Permutation generated by rotating all the elements to the left.
rotateLeft :: KnownNat n => Int -> Permutation n
rotateLeft n = mkPermutation $ \i -> i+n

-- | Permutation generated by inverting another permutation.
invert :: Permutation n -> Permutation n
invert (P v) = P (V.accumulate_ (\_ new -> new) initial v iota)
  where
    n       = V.length v
    initial = V.replicate n 0 -- 0 is arbitrary, should all be overwritten
    iota    = V.generate n id

-- | Permutation generated by reversing the order of the elements.
backwards :: KnownNat n => Permutation n
backwards = mkPermutation $ \i -> -i-1

-- | Compute the disjoint cycles of the permutation.
--
-- >>> cycles (swap 1 2 <> swap 3 4 <> swap 4 5 :: Permutation 6)
-- [[0],[1,2],[3,4,5]]
cycles :: Permutation n -> [[Int]]
cycles (P v) = unfoldr aux initialSet
  where
    initialSet = IntSet.fromList [0 .. V.length v-1]

    getOne start cur
      | start == cur = []
      | otherwise    = cur : getOne start (v V.! cur)

    aux items =
      do (seed, items') <- IntSet.minView items
         let cycleElts = seed : getOne seed (v V.! seed)
         return (cycleElts, foldl' (flip IntSet.delete) items' cycleElts)

-- | Compute the order of a permutation.
--
-- >>> order (swap 1 2 <> swap 3 4 <> swap 4 5 :: Permutation 6)
-- 6
order :: Permutation n -> Int
order = foldl' lcm 1 . map length . cycles

-- | @a '<>' b@ is the permutation that first permutes with @a@ and
-- then with @b@.
instance Semigroup (Permutation n) where
  P x <> P y        = P (V.backpermute x y)
  sconcat (x :| xs) = foldl' (<>) x xs

instance KnownNat n => Monoid (Permutation n) where
  mempty           = mkPermutation id
  mconcat []       = mempty
  mconcat (x : xs) = sconcat (x :| xs)

instance KnownNat n => Group (Permutation n) where
  inverse = invert
