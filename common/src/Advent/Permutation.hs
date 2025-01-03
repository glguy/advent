{-# Language ImportQualifiedPost, RoleAnnotations, DataKinds, KindSignatures, BlockArguments, ScopedTypeVariables #-}
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
  , index
  ) where

import Advent.Group (Group(..))
import Data.Function (fix)
import Data.IntSet qualified as IntSet
import Data.List (foldl', unfoldr)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (Semigroup(sconcat))
import Data.Vector.Unboxed qualified as V
import GHC.TypeLits (KnownNat, Nat, natVal)

-- $setup
-- >>> :set -XDataKinds

type role Permutation nominal

-- | Permutations of @n@ elements
newtype Permutation (n :: Nat) = P (V.Vector Int)
  deriving (Eq, Ord)

-- | Produce a list mapping list elements from their indexes to their output elements.
runPermutation :: (Int -> a) -> Permutation n -> [a]
runPermutation f (P v) = f <$> V.toList v
{-# Inline runPermutation #-}

-- | Size of the list of elements permuted.
--
-- >>> size (mempty :: Permutation 5)
-- 5
size :: Permutation n -> Int
size (P v) = V.length v

-- | Validate a permutation. A valid permutation will map each element in the input
-- to a unique element in the output.
--
-- >>> isValid (mempty :: Permutation 5)
-- True
--
-- >>> isValid (mkPermutation (const 0) :: Permutation 5)
-- False
isValid :: KnownNat n => Permutation n -> Bool
isValid p@(P v) =
  natVal p == fromIntegral n &&
  V.and (V.update_ (V.replicate n False) v (V.replicate n True))
  where
    n = size p

-- | Helper function for making the size of a requested permutation available
-- while building the permutation.
withSize :: KnownNat n => (Int -> Permutation n) -> Permutation n
withSize f = fix (f . fromIntegral . natVal)

-- | Given a function mapping incoming indices to outgoing ones, construct
-- a new permutation value.
mkPermutation :: KnownNat n => (Int -> Int) -> Permutation n
mkPermutation f = withSize \n -> P (V.generate n \i -> f i `mod` n)

-- | Permutation generated by swapping the elements at a pair of indices.
--
-- >>> swap 2 3 :: Permutation 5
-- [0,1,3,2,4]
swap :: KnownNat n => Int -> Int -> Permutation n
swap x y = withSize \n ->
  let x' = x `mod` n -- not evaluated when n == 0
      y' = y `mod` n
  in mkPermutation \i -> if i == x' then y' else if i == y' then x' else i

-- | Permutation generated by rotating all the elements to the right.
--
-- >>> rotateRight 2 :: Permutation 5
-- [3,4,0,1,2]
rotateRight :: KnownNat n => Int -> Permutation n
rotateRight = mkPermutation . subtract
{-# INLINE rotateRight #-}

-- | Permutation generated by rotating all the elements to the left.
--
-- >>> rotateLeft 2 :: Permutation 5
-- [2,3,4,0,1]
rotateLeft :: KnownNat n => Int -> Permutation n
rotateLeft = mkPermutation . (+)
{-# INLINE rotateLeft #-}

-- | Permutation generated by inverting another permutation.
--
-- >>> swap 1 2 <> swap 3 4 <> swap 4 5 :: Permutation 6
-- [0,2,1,4,5,3]
--
-- >>> invert (swap 1 2 <> swap 3 4 <> swap 4 5 :: Permutation 6)
-- [0,2,1,5,3,4]
invert :: Permutation n -> Permutation n
invert (P v) = P (V.update_ initial v iota)
  where
    n       = V.length v
    initial = V.replicate n 0 -- 0 is arbitrary, should all be overwritten
    iota    = V.generate n id

-- | Permutation generated by reversing the order of the elements.
--
-- >>> backwards :: Permutation 4
-- [3,2,1,0]
backwards :: KnownNat n => Permutation n
backwards = mkPermutation \i -> -i - 1
{-# INLINE backwards #-}

-- | Compute the disjoint cycles of the permutation.
--
-- >>> cycles (swap 1 2 <> swap 3 4 <> swap 4 5 :: Permutation 6)
-- [[0],[1,2],[3,4,5]]
cycles :: Permutation n -> [[Int]]
cycles p = unfoldr aux initialSet
  where
    initialSet = IntSet.fromList [0 .. size p - 1]

    getOne start cur
      | start == cur = []
      | otherwise    = cur : getOne start (index p cur)

    aux items =
     do (seed, items') <- IntSet.minView items
        let cycleElts = seed : getOne seed (index p seed)
        pure (cycleElts, foldl' (flip IntSet.delete) items' cycleElts)

-- | Find the output element for the corresponding input element.
index :: Permutation n -> Int -> Int
index (P p) i = p V.! i

-- | Compute the order of a permutation.
--
-- >>> order (swap 1 2 <> swap 3 4 <> swap 4 5 :: Permutation 6)
-- 6
order :: Permutation n -> Int
order = foldl' lcm 1 . map length . cycles

-- | @a '<>' b@ is the permutation that first permutes with @a@ and
-- then with @b@.
--
-- >>> swap 1 2 <> swap 3 4 <> swap 4 5 :: Permutation 6
-- [0,2,1,4,5,3]
instance Semigroup (Permutation n) where
  P x <> P y = P (V.backpermute x y)
  sconcat (x :| xs) = foldl' (<>) x xs

-- | Extend the 'Semigroup' instance with an identity permutation as 'mempty'.
-- 
-- >>> mempty :: Permutation 6
-- [0,1,2,3,4,5]
instance KnownNat n => Monoid (Permutation n) where
  mempty           = mkPermutation id
  mconcat []       = mempty
  mconcat (x : xs) = sconcat (x :| xs)

-- | Extends the 'Monoid' instance using 'invert'
instance KnownNat n => Group (Permutation n) where
  inverse = invert

-- | Render a permutation as a list literal.
--
-- >>> show (mempty :: Permutation 4)
-- "[0,1,2,3]"
instance Show (Permutation n) where
  show (P p) = show p

-- | Parse a permutation as a list literal.
instance KnownNat n => Read (Permutation n) where
  readsPrec p str =
    [ (p, str')
    | (xs, str') <- readsPrec p str
    , let p = P xs :: Permutation n
    , isValid p
    ]
