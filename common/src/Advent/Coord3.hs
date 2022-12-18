{-# Language DeriveGeneric, DeriveDataTypeable #-}
{-|
Module      : Advent.Coord3
Description : 3D coordinates
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Coord3 where

import Data.Data (Data)
import GHC.Generics (Generic)
import GHC.Ix (Ix(unsafeIndex, range, index, inRange, unsafeRangeSize), indexError)

-- | Three-dimensional coordinate: x, y, z
data Coord3 = C3 !Int !Int !Int
  deriving (Eq, Ord, Show, Generic, Data)

-- | Alias for @0, 0, 0@
origin :: Coord3
origin = C3 0 0 0

-- | Sum of absolute value of differences in each of the 3 axes.
manhattan :: Coord3 -> Coord3 -> Int
manhattan (C3 x1 y1 z1) (C3 x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

-- | Find the upper-left and lower-right coordinates that
-- inclusively contain all the coordinates in a list of
-- coordinates.
boundingBox :: [Coord3] -> Maybe (Coord3, Coord3)
boundingBox t =
  case t of
    []            -> Nothing
    C3 x y z : cs -> go x y z x y z cs
  where
    go lox loy loz hix hiy hiz [] =
        lo `seq` hi `seq` Just (lo, hi)
        where
          lo = C3 lox loy loz
          hi = C3 hix hiy hiz
    go lox loy loz hix hiy hiz (C3 x y z : cs) =
        go (min lox x) (min loy y) (min loz z) (max hix x) (max hiy y) (max hiz z) cs

-- | Apply a function to the y and x coordinate
mapCoord :: (Int -> Int) -> Coord3 -> Coord3
mapCoord f (C3 x y z) = C3 (f x) (f y) (f z)

-- | Use a function pairwise on x and y coordinates of the two arguments
zipCoord :: (Int -> Int -> Int) -> Coord3 -> Coord3 -> Coord3
zipCoord f (C3 x1 y1 z1) (C3 x2 y2 z2) = C3 (f x1 x2) (f y1 y2) (f z1 z2)

-- | Vector arithmetic
instance Num Coord3 where
  (+) = zipCoord (+)
  {-# INLINE (+) #-}
  (-) = zipCoord (-)
  {-# INLINE (-) #-}
  (*) = zipCoord (*)
  {-# INLINE (*) #-}
  negate = mapCoord negate
  {-# INLINE negate #-}
  abs = mapCoord abs
  {-# INLINE abs #-}
  signum = mapCoord signum
  {-# INLINE signum #-}
  fromInteger = (\i -> C3 i i i) . fromInteger
  {-# INLINE fromInteger #-}

instance Ix Coord3 where
    range (C3 l1 l2 l3, C3 u1 u2 u3) =
        [C3 i1 i2 i3 
                | i1 <- range (l1,u1),
                  i2 <- range (l2,u2),
                  i3 <- range (l3,u3)]

    unsafeIndex (C3 l1 l2 l3, C3 u1 u2 u3) (C3 i1 i2 i3) =
      unsafeIndex (l3,u3) i3 + unsafeRangeSize (l3,u3) * (
      unsafeIndex (l2,u2) i2 + unsafeRangeSize (l2,u2) * (
      unsafeIndex (l1,u1) i1))

    inRange (C3 l1 l2 l3, C3 u1 u2 u3) (C3 i1 i2 i3) =
      inRange (l1,u1) i1 && inRange (l2,u2) i2 &&
      inRange (l3,u3) i3