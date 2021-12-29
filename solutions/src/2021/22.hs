{-# Language KindSignatures, GADTs, DataKinds, ParallelListComp, MonadComprehensions, BlockArguments, TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/22>

This problem is made simple by processing commands
by subtracting away all future cuboids. Only the region
unique to the current command will affect the final output.

-}
module Main (main) where

import Advent.Format (format)
import Control.Monad.Trans.Writer.CPS (runWriterT, writerT)
import Data.Kind (Type)
import Data.Maybe (isNothing, mapMaybe)
import Data.Monoid (Any(Any))

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
    let seg lo hi = Seg lo (hi+1) -- make upper limit exclusive
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

-- | Split up the input segment, if needed, at the terminal points of the extra
-- segment.
--
-- >>> cutSeg (Seg 2 4) (Seg 0 9)
-- [Seg 0 2,Seg 2 4,Seg 4 9]
--
-- >>> cutSeg (Seg 0 5) (Seg 2 7)
-- [Seg 2 5,Seg 5 7]
--
-- >>> cutSeg (Seg 0 3) (Seg 5 8)
-- [Seg 5 8]
--
-- >>> cutSeg (Seg 0 9) (Seg 3 6)
-- [Seg 3 6]
cutSeg ::
  Seg {- ^ extra points -} ->
  Seg {- ^ input segment -} ->
  [Seg] {- ^ split up input segment -}
cutSeg (Seg a b) (Seg c d) = [Seg lo hi | lo <- pts | hi <- tail pts]
  where
    pts = [c] ++ [a | c < a, a < d] ++ [b | c < b, b < d] ++ [d]

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
intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox = traverseBox2 intersectSeg

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
-- [Seg 0 1 :* Seg 1 2 :* Pt,Seg 1 2 :* Seg 0 1 :* Pt,Seg 1 2 :* Seg 1 2 :* Pt]
--
-- >>> subtractBox (Seg 0 9 :* Pt) (Seg 3 6 :* Pt)
-- []
subtractBox ::
  Box n {- ^ remove this -} ->
  Box n {- ^ from this -} ->
  [Box n] {- ^ leaving these -}
subtractBox b1 b2
  | isNothing (intersectBox b1 b2) = [b2]
  | otherwise = [b | (b, Any True) <- runWriterT (traverseBox2 segs b1 b2)]
  where
    segs s1 s2 = writerT [(s, Any (isNothing (intersectSeg s s1))) | s <- cutSeg s1 s2]

-- | Zip two boxes together.
traverseBox2 :: Applicative f => (Seg -> Seg -> f Seg) -> Box n -> Box n -> f (Box n)
traverseBox2 f (x :* xs) (y :* ys) = (:*) <$> f x y <*> traverseBox2 f xs ys
traverseBox2 _ Pt        Pt        = pure Pt
