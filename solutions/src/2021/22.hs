{-# Language KindSignatures, GADTs, DataKinds, ParallelListComp, BlockArguments, TemplateHaskell, ImportQualifiedPost, QuasiQuotes #-}
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
import Data.Monoid (All(All))

-- | On and off commands from the input file
data C = Con | Coff
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
applyCommand ons (c, b) = [b | Con == c] ++ concatMap (subBox b) ons

-- * Segments

-- | A segment defined by an inclusive lower-bound and an exclusive upper-bound.
data Seg = Seg !Int !Int deriving (Eq, Ord, Show)

-- | Compute the length of a segment
len :: Seg -> Int
len (Seg lo hi) = hi - lo

-- | Determine if two segments have some overlap
intersectSeg :: Seg -> Seg -> Maybe Seg
intersectSeg (Seg alo ahi) (Seg blo bhi)
  | lo < hi = Just (Seg lo hi)
  | otherwise = Nothing
  where
    lo = max alo blo
    hi = min ahi bhi

-- | Determine if a value falls within a segment.
inSeg :: Int -> Seg -> Bool
inSeg x (Seg lo hi) = lo <= x && x < hi

-- * N-dimensional boxes

-- | Natural numbers (used for type index)
data N = S N | Z

-- | An n-dimensional box.
data Box :: N -> Type where
  Pt   :: Box 'Z -- ^ A single point
  (:*) :: Seg -> Box n -> Box ('S n) -- ^ A box extended along an axis

infixr 6 :* -- a little higher than list cons

instance Show (Box n) where
  showsPrec _ Pt = showString "Pt"
  showsPrec p (x :* xs) = showParen (p > 6) (shows x . showString " :* " . showsPrec 6 xs)

-- | Returns the number of points contained in a box.
--
-- >>> size (Seg 1 4 :* Seg 0 3 :* Seg 0 2 :* Pt)
-- 18
size :: Box n -> Int
size Pt         = 1
size (s :* box) = len s * size box

-- | The intersection of two boxes is the intersection of their segments.
intersectBox :: Box n -> Box n -> Maybe (Box n)
intersectBox = traverseBox2 intersectSeg

-- | Subtract the second box from the first box returning a list of boxes
-- that cover all the remaining area.
--
-- >>> subBox (Seg 2 3 :* Pt) (Seg 0 4 :* Pt)
-- [Seg 0 2 :* Pt,Seg 3 4 :* Pt]
--
-- >>> subBox (Seg 3 5 :* Pt) (Seg 0 4 :* Pt)
-- [Seg 0 3 :* Pt]
subBox ::
  Box n {- ^ remove this -} ->
  Box n {- ^ from this -} ->
  [Box n]
subBox b1 b2
  | isNothing (intersectBox b1 b2) = [b2]
  | otherwise = [b | (b, All False) <- runWriterT (traverseBox2 segs b1 b2)]
  where
    segs s1@(Seg a b) s2@(Seg c d) =
      writerT [(Seg lo hi, All (lo `inSeg` s1)) | lo <- xs | hi <- tail xs]
      where
        xs = [c] ++ [a | a `inSeg` s2] ++ [b | b `inSeg` s2] ++ [d]

-- | Zip two boxes together.
traverseBox2 :: Applicative f => (Seg -> Seg -> f Seg) -> Box n -> Box n -> f (Box n)
traverseBox2 f (x :* xs) (y :* ys) = (:*) <$> f x y <*> traverseBox2 f xs ys
traverseBox2 _ Pt        Pt        = pure Pt
