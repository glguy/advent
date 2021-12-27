{-# Language ImportQualifiedPost, DeriveGeneric #-}
{-|
Module      : Advent.Coord3
Description : 3D coordinates
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Coord3 where

data Coord3 = C3 !Int !Int !Int deriving (Eq, Ord, Show)

origin :: Coord3
origin = C3 0 0 0

manhattan :: Coord3 -> Coord3 -> Int
manhattan (C3 x1 y1 z1) (C3 x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

diff :: Coord3 -> Coord3 -> Coord3
diff (C3 x y z) (C3 x' y' z') = C3 (x-x') (y-y') (z-z')

add :: Coord3 -> Coord3 -> Coord3
add  (C3 x y z) (C3 x' y' z') = C3 (x+x') (y+y') (z+z')
