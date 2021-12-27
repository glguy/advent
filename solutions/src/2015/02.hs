{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/2>

Computes volumes and surface areas of boxes.

-}
module Main where

import Advent (format)
import Data.List (sort)

data Package = Package Int Int Int
data Face = Face Int Int

main :: IO ()
main =
 do input <- [format|2 (%ux%ux%u%n)*|]
    let packages = [Package x y z | (x,y,z) <- input]   
    print (sum (part1 <$> packages))
    print (sum (part2 <$> packages))

part1 :: Package -> Int
part1 p = surfaceArea p + area (smallestFace p)

part2 :: Package -> Int
part2 p = volume p + perimeter (smallestFace p)

volume :: Package -> Int
volume (Package x y z) = x*y*z

surfaceArea :: Package -> Int
surfaceArea (Package x y z) = 2 * (x*y + x*z + y*z)

smallestFace :: Package -> Face
smallestFace (Package x y z) = let a:b:_ = sort [x,y,z] in Face a b

area :: Face -> Int
area (Face x y) = x*y

perimeter :: Face -> Int
perimeter (Face x y) = 2*(x+y)
