{-# Language QuasiQuotes, MonadComprehensions, BangPatterns, LambdaCase, ImportQualifiedPost, BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/24>

-}
module Main (main) where

import Advent (format)
import Data.List (tails)
import Advent.Coord3 (Coord3(C3))
import Numeric.LinearAlgebra (Matrix, linearSolve, atIndex, (><))
import Text.Printf (printf)
import Control.Monad (zipWithM_)

main :: IO ()
main =
 do input_ <- [format|2023 24 ( *%d, *%d, *%d *%@ *%d, *%d, *%d%n)*|]
    let input = [(C3 x y z, C3 dx dy dz) | (x,y,z,dx,dy,dz) <- input_]
    print $ length [(x,y, int2 x y)
            | x:xs <- tails input, y <- xs
            , Just (a,b) <- [int2 x y]
            , testArea a
            , testArea b
            ]
    render input -- just dumped these out to Z3

testArea :: Double -> Bool
testArea x =
  200000000000000 <= x && x <= 400000000000000

int2 :: (Coord3, Coord3) -> (Coord3, Coord3) -> Maybe (Double, Double)
int2 (C3 x1 y1 _, C3 dx1 dy1 _) (C3 x2 y2 _, C3 dx2 dy2 _) =
 [ (x,y)
    | tu <- linearSolve a b
    , let t = tu `atIndex` (0,0)
          u = tu `atIndex` (1,0)
          x = fromIntegral x1 + fromIntegral dx1 * t
          y = fromIntegral y1 + fromIntegral dy1 * t
    , t >= 0, u >= 0
 ]
  where
    a :: Matrix Double
    a = (2><2) [ - fromIntegral dx1, fromIntegral dx2 ,
                 - fromIntegral dy1, fromIntegral dy2 ]
    b = (2><1) [ fromIntegral (x1 - x2) , fromIntegral (y1 - y2) ]

render :: [(Coord3, Coord3)] -> IO ()
render = zipWithM_ draw [0..2::Int]
  where
    draw :: Int -> (Coord3, Coord3) -> IO ()
    draw i (C3 x y z, C3 dx dy dz) =
     do draw1 "x" i x dx
        draw1 "y" i y dy
        draw1 "z" i z dz
    draw1 n i p v =
      printf "%s - %d + t%d * (d%s - %d) = 0\n" n p i n v
