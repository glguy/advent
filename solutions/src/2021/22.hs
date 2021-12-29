{-# Language StandaloneDeriving, KindSignatures, GADTs, DataKinds, ParallelListComp, MonadComprehensions, TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/22>

This solution processes commands in order and tracks the
currently lit regions with a list of disjoint cubes. When
processing a new cube, the cube is deleted from the list of
lit cubes and then added back in as a whole cube if the command
would turn lights on.

-}
module Main (main) where

import Advent.Format (format)
import Data.Kind (Type)
import Data.Maybe (mapMaybe)

-- | On and off commands from the input file
data C = Con {- ^ lights on -} | Coff {- ^ lights off -}
  deriving (Show, Eq, Ord)

mempty -- template haskell staging

-- | >>> :main
-- 606484
-- 1162571910364852
main :: IO ()
main =
 do inp <- [format|22 (@C x=%d..%d,y=%d..%d,z=%d..%d%n)*|]
    let seg lo hi = Seg lo (hi+1) -- make upper bound exclusive
        steps = [ (c, seg x1 x2 :* seg y1 y2 :* seg z1 z2 :* Pt)
                | (c, x1, x2, y1, y2, z1, z2) <- inp]
        p1seg = seg (-50) 50
        p1cube = p1seg :* p1seg :* p1seg :* Pt
    print (solve (mapMaybe (traverse (intersectBox p1cube)) steps))
    print (solve steps)

-- | Figure out how many lights the given instructions turn on.
solve :: [(C, Box n)] -> Int
solve = sum . map size . foldl applyCommand []

-- | Apply a command given a list of non-overlapping, illuminated regions.
applyCommand ::
  [Box n]    {- ^ pre-lit boxes  -} ->
  (C, Box n) {- ^ command        -} ->
  [Box n]    {- ^ post-lit boxes -}
applyCommand ons (c, b) = [b | Con == c] ++ concatMap (subtractBox b) ons

-- * Segments

-- | A segment defined by an inclusive lower-bound and an exclusive upper-bound.
data Seg = Seg !Int !Int deriving (Eq, Ord, Show)

-- | Compute the length of a segment
--
-- >>> len (Seg 3 7)
-- 4
len :: Seg -> Int
len (Seg lo hi) = hi - lo

-- | Determine if two segments have some overlap
--
-- >>> intersectSeg (Seg 0 1) (Seg 1 2)
-- Nothing
--
-- >>> intersectSeg (Seg 0 10) (Seg 3 6)
-- Just (Seg 3 6)
--
-- >>> intersectSeg (Seg 0 6) (Seg 3 9)
-- Just (Seg 3 6)
intersectSeg :: Seg -> Seg -> Maybe Seg
intersectSeg (Seg alo ahi) (Seg blo bhi) = [Seg lo hi | lo < hi]
  where
    lo = max alo blo
    hi = min ahi bhi

-- * N-dimensional boxes

-- | Natural numbers (used for type index)
data N
  = Z   -- ^ zero
  | S N -- ^ successor

-- | An n-dimensional box.
data Box :: N -> Type where
  Pt   ::                 Box 'Z     -- ^ A single point
  (:*) :: Seg -> Box n -> Box ('S n) -- ^ A box extended along an axis

infixr 6 :* -- a little higher than list cons

deriving instance Eq (Box n)

-- | Custom 'Show' instance avoids redundant parentheses
instance Show (Box n) where
  showsPrec _ Pt        = showString "Pt"
  showsPrec p (x :* xs) = showParen (p > 6) (shows x . showString " :* " . showsPrec 6 xs)

-- | Returns the number of points contained in a box.
--
-- >>> size Pt -- 0D point
-- 1
--
-- >>> size (Seg 1 4 :* Pt) -- 1D segment; length
-- 3
--
-- >>> size (Seg 1 4 :* Seg 0 3 :* Pt) -- 2D rectangle; area
-- 9
--
-- >>> size (Seg 1 4 :* Seg 0 3 :* Seg 0 2 :* Pt) -- 3D cuboid; volume
-- 18
size :: Box n -> Int
size Pt         = 1
size (s :* box) = len s * size box

-- | The intersection of two boxes is the intersection of their segments.
--
-- >>> intersectBox (Seg 0 2 :* Seg 0 3 :* Pt) (Seg 1 4 :* Seg 2 4 :* Pt)
-- Just (Seg 1 2 :* Seg 2 3 :* Pt)
intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox (x :* xs) (y :* ys) = (:*) <$> intersectSeg x y <*> intersectBox xs ys
intersectBox Pt        Pt        = Just Pt

-- | Subtract the first box from the second box returning a list of boxes
-- that cover all the remaining area.
--
-- >>> subtractBox (Seg 2 3 :* Pt) (Seg 0 4 :* Pt)
-- [Seg 0 2 :* Pt,Seg 3 4 :* Pt]
--
-- >>> subtractBox (Seg 3 5 :* Pt) (Seg 0 4 :* Pt)
-- [Seg 0 3 :* Pt]
--
-- >>> subtractBox (Seg 0 1 :* Pt) (Seg 1 2 :* Pt)
-- [Seg 1 2 :* Pt]
--
-- >>> subtractBox (Seg 0 1 :* Seg 0 1 :* Pt) (Seg 0 2 :* Seg 0 2 :* Pt)
-- [Seg 1 2 :* Seg 0 2 :* Pt,Seg 0 1 :* Seg 1 2 :* Pt]
--
-- >>> subtractBox (Seg 0 9 :* Pt) (Seg 3 6 :* Pt)
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
subtractBox' (s@(Seg a b) :* is) (Seg c d :* ys)
    = [Seg c a :* ys | c < a] ++
      [Seg b d :* ys | b < d] ++
      map (s :*) (subtractBox' is ys)
