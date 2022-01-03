{-# Language StandaloneDeriving, KindSignatures, GADTs, DataKinds, MonadComprehensions, TemplateHaskell, ImportQualifiedPost, QuasiQuotes, ViewPatterns #-}
{-|
Module      : Advent.Box
Description : N-dimensional boxes
Copyright   : 2021 Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com

This module expresses boxes as a list of bounds on each axis. This representation
enables efficient intersection and subtraction operations.

-}
module Advent.Box where

import Advent.Nat (Nat(S,Z))
import Control.Monad (foldM)
import Data.Kind (Type)
import GHC.Stack (HasCallStack)

-- | An n-dimensional box.
data Box :: Nat -> Type where
  Pt  ::  Box 'Z -- ^ A single point
  Dim ::  !Int {- ^ inclusive lower bound -} ->
          !Int {- ^ exclusive upper bound -} ->
          Box n {- ^ lower dimensional box -} ->
          Box ('S n) -- ^ A box extended along an axis

deriving instance Show (Box n)
deriving instance Eq (Box n)
deriving instance Ord (Box n)

-- | Returns the number of points contained in a box.
--
-- >>> size Pt -- 0D point
-- 1
--
-- >>> size (Dim 1 4 Pt) -- 1D segment; length
-- 3
--
-- >>> size (Dim 1 4 (Dim 0 3 Pt)) -- 2D rectangle; area
-- 9
--
-- >>> size (Dim 1 4 (Dim 0 3 (Dim 0 2 Pt))) -- 3D cuboid; volume
-- 18
size :: Box n -> Int
size Pt              = 1
size (Dim lo hi box) = (hi - lo) * size box

-- | The intersection of two boxes is the intersection of their segments.
--
-- >>> intersectBox (Dim 0 2 (Dim 0 3 Pt)) (Dim 1 4 (Dim 2 4 Pt))
-- Just (Dim 1 2 (Dim 2 3 Pt))
intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox Pt Pt = Just Pt
intersectBox (Dim a b xs) (Dim c d ys) =
  [Dim x y zs | let x = max a c, let y = min b d, x < y, zs <- intersectBox xs ys]

-- | Intersection of one or more boxes.
intersectBoxes :: HasCallStack => [Box n] -> Maybe (Box n)
intersectBoxes []     = error "intersectBoxes: empty intersection"
intersectBoxes (x:xs) = foldM intersectBox x xs

-- | Subtract the first box from the second box returning a list of boxes
-- that cover all the remaining area.
--
-- >>> subtractBox (Dim 2 3 Pt) (Dim 0 4 Pt)
-- [Dim 0 2 Pt,Dim 3 4 Pt]
--
-- >>> subtractBox (Dim 3 5 Pt) (Dim 0 4 Pt)
-- [Dim 0 3 Pt]
--
-- >>> subtractBox (Dim 0 1 Pt) (Dim 1 2 Pt)
-- [Dim 1 2 Pt]
--
-- >>> subtractBox (Dim 0 1 (Dim 0 1 Pt)) (Dim 0 2 (Dim 0 2 Pt))
-- [Dim 1 2 (Dim 0 2 Pt),Dim 0 1 (Dim 1 2 Pt)]
--
-- >>> subtractBox (Dim 0 9 Pt) (Dim 3 6 Pt)
-- []
subtractBox ::
  Box n {- ^ remove this -} ->
  Box n {- ^ from this -} ->
  [Box n] {- ^ leaving these -}
subtractBox b1 b2 =
  case intersectBox b1 b2 of
    Nothing -> [b2]
    Just b  -> subtractBox' b b2

-- | Worker for 'subtractBox' where the first argument is a
-- subset of the second argument.
subtractBox' :: Box n -> Box n -> [Box n]
subtractBox' Pt Pt = []
subtractBox' (Dim a b xs) (Dim c d ys) =
  [Dim c a ys | c < a] ++
  [Dim b d ys | b < d] ++
  [Dim a b zs | zs <- subtractBox' xs ys]
