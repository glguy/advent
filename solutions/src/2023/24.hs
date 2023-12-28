{-# Language QuasiQuotes, NumericUnderscores, MonadComprehensions, BlockArguments #-}
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
import Data.Foldable (for_)
import Data.SBV (Symbolic, AlgReal, SReal, free, sat, (.==), constrain, getModelValue, SatResult(SatResult))

main :: IO ()
main =
 do input_ <- [format|2023 24 ( *%d, *%d, *%d *%@ *%d, *%d, *%d%n)*|]
    let input = [(C3 x y z, C3 dx dy dz) | (x,y,z,dx,dy,dz) <- input_]
    print $ length [()
            | x:xs <- tails input, y <- xs
            , Just (a,b) <- [int2 x y]
            , testArea a
            , testArea b
            ]

    SatResult res <- sat
     do x  <- free "x" :: Symbolic SReal
        y  <- free "y"
        z  <- free "z"
        dx <- free "dx"
        dy <- free "dy"
        dz <- free "dz"
        for_ (take 3 input) \(C3 x_ y_ z_, C3 dx_ dy_ dz_) ->
         do t <- free "t"
            constrain (t * (dx - fromIntegral dx_) .== (fromIntegral x_ - x))
            constrain (t * (dy - fromIntegral dy_) .== (fromIntegral y_ - y))
            constrain (t * (dz - fromIntegral dz_) .== (fromIntegral z_ - z))

    case (getModelValue "x" res, getModelValue "y" res, getModelValue "z" res) of
      (Just x, Just y, Just z) -> print (x+y+z :: AlgReal)
      _ -> fail "no solution"

testArea :: Double -> Bool
testArea x = 200_000_000_000_000 <= x && x <= 400_000_000_000_000

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
