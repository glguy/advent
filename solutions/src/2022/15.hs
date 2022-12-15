{-# Language QuasiQuotes, DataKinds, NumericUnderscores, GADTs #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/15>

-}
module Main where

import Data.List (sort)

import Advent ( format )
import Advent.Box
import Advent.Coord ( manhattan, Coord(C) )
import Advent.Nat ( Nat(Z, S) )

type Input = [(Int,Int,Int,Int)]

-- |
-- >>> :main
-- 4724228
-- 13622251246513
main :: IO ()
main = do
    input <- [format|2022 15 (Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d%n)*|]

    let p1y = 2_000_000
    print $ sum $ map size $
        removeallof (beaconsAtY input p1y) $
        boxUnion [y | x <- input, y <- ranges p1y x]
    
    print [
      4_000_000 * y + x
      | C y x <- 
          map fromdiamond $
          removeallof (todiamonds input)
          [todiamond (C 2000000 2000000) 4000000 ]
      , 0 <= y, y <= 4000000, 0 <= x , x <= 4000000]

fromdiamond :: Box ('S ('S 'Z)) -> Coord
fromdiamond (Dim _ b (Dim _ d Pt)) = C ((b + d)`div`2) ((b-d)`div`2)
  
todiamonds :: Input -> [Box ('S ('S 'Z))]
todiamonds input =
  [ todiamond (C y x) r
     | (x,y,nx,ny) <- input
     , let r = manhattan (C y x) (C ny nx)
     ]

todiamond :: Coord -> Int -> Box ('S ('S 'Z))
todiamond (C y x) r =  Dim (a - r) (a + r + 1) (Dim (b - r) (b + r + 1) Pt)
  where
    a = x + y
    b = x - y
      
beaconsAtY :: Input -> Int -> [Box ('S 'Z)]
beaconsAtY input ty = [Dim nx (nx+1) Pt | (_,_,nx,ny)<-input, ny == ty]

ranges :: Int -> (Int,Int,Int,Int) -> [Box ('S 'Z)]
ranges yy (x,y,nx,ny) 
  | dx < 0 = []
  | otherwise = [Dim (x - dx) (x + dx+1) Pt]
    where
        dy = abs (yy - y)
        dx = dist - dy
        dist = manhattan (C y x) (C ny nx)

-- Box helpers

consolidate :: [Box ('S 'Z)] -> [Box ('S 'Z)]
consolidate = aux . sort
  where
    aux (Dim a b Pt : Dim c d Pt : xs) | b == c = aux (Dim a d Pt : xs)
    aux (x:xs) = x : aux xs
    aux [] = []

removeallof :: [Box n] -> [Box n] -> [Box n]
removeallof xs ys = foldl remove1 ys xs
 where remove1 acc x = concatMap (subtractBox x) acc

boxUnion :: [Box a] -> [Box a]
boxUnion = foldr add []
  where
    add x sofar = x : concatMap (subtractBox x) sofar
