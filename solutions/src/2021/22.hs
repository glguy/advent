{-# Language StandaloneDeriving, KindSignatures, GADTs, DataKinds, MonadComprehensions, TemplateHaskell, ImportQualifiedPost, QuasiQuotes, ViewPatterns #-}
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

A slower, but also popular solution using the Inclusion-Exclusion Principle
can be found in 'solveInEx'.

<https://en.wikipedia.org/wiki/Inclusion%E2%80%93exclusion_principle>

-}
module Main (main) where
import Advent.Format (format)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Foldable (foldl')
-- | On and off commands from the input file
data C = Con {- ^ lights on -} | Coff {- ^ lights off -}
  deriving (Show, Eq)

mempty -- template haskell staging

-- | >>> :main
-- 606484
-- 1162571910364852
main :: IO ()
main =
 do inp <- [format|22 (@C x=%d..%d,y=%d..%d,z=%d..%d%n)*|]
    let dim lo hi = Dim lo (hi+1) -- make upper bound exclusive
        steps = [ (c, dim x1 x2 (dim y1 y2 (dim z1 z2 Pt)))
                | (c, x1, x2, y1, y2, z1, z2) <- inp]
        p1dim = dim (-50) 50
        p1box = p1dim (p1dim (p1dim Pt))
    print (solve [(c, b) | (c, intersectBox p1box -> Just b) <- steps])
    print (solve steps)

{-
incexc :: [(C,Box n)] -> Int
incexc cmds =
  case foldl step ([],[]) cmds of
    (ons, offs) -> sum (map size ons) - sum (map size offs)
  where
    step (ons, offs) (c, box) =
      ([ box | c == Con] ++ mapMaybe (intersectBox box) offs ++ ons,
                            mapMaybe (intersectBox box) ons ++ offs)
-}

-- | Figure out how many lights the given instructions turn on.
solve :: [(C, Box n)] -> Int
solve = sum . map size . foldl applyCommand []

-- | Apply a command given a list of non-overlapping, illuminated regions.
applyCommand ::
  [Box n]    {- ^ pre-lit boxes  -} ->
  (C, Box n) {- ^ command        -} ->
  [Box n]    {- ^ post-lit boxes -}
applyCommand ons (c, b) = [b | Con == c] ++ (subtractBox b =<< ons)

-- * N-dimensional boxes

-- | Natural numbers (used for type index)
data N
  = Z   -- ^ zero
  | S N -- ^ successor

-- | An n-dimensional box.
data Box :: N -> Type where
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

-- * Alternative solution

-- | This solution uses the inclusion-exclusion principle instead
-- of relying on 'subtractBox'
solveInEx :: [(C, Box n)] -> Int
solveInEx cmds =
  sum [size k * v | (k,v) <- Map.assocs (foldl' addCommandInEx Map.empty cmds)]

-- | Update the inclusion and exclusion regions based on a single command.
-- This uses the same logic as 'solve' in that it turns off all the lights
-- in the given region first and then it turns them back on if the command
-- is /on/ and leaves them off if the command is /off/.
addCommandInEx ::
  Map (Box n) Int {- ^ inclusions and exclusions -} ->
  (C, Box n)      {- ^ command and region        -} ->
  Map (Box n) Int {- ^ inclusions and exclusions -}
addCommandInEx boxes (cmd, box)
  = Map.filter (0 /=) -- optimization to avoid finding intersections that don't matter
  $ Map.unionWith (+) boxes
  $ Map.fromListWith (+)
  $ [(box,1) | cmd == Con] ++
    [(k, -v) | (intersectBox box -> Just k,v) <- Map.assocs boxes] 
