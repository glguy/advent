{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/18>

-}
module Main where

import Advent (countBy, arrIx, times, getInputArray)
import Advent.Coord (Coord(C), neighbors)
import Data.Array.Unboxed (Ix(range), IArray(bounds), UArray, (!), amap, array, elems)

type Lights = UArray Coord Bool

main :: IO ()
main =
  do input <- amap ('#'==) <$> getInputArray 2015 18
     print $ countLights $ times 100 (applyRule life) input
     print $ countLights $ times 100 (applyRule (addCorners life)) input

countLights :: Lights -> Int
countLights = countBy id . elems                  

type Rule = Lights -> Coord -> Bool

applyRule :: Rule -> Lights -> Lights
applyRule f a = array (bounds a) [(i, f a i) | i <- range (bounds a)] 

life :: Rule
life a c = n == 3 ||
           n == 2 && a!c
  where
    n = countBy (\x -> arrIx a x == Just True) (neighbors c)

addCorners :: Rule -> Rule
addCorners f a i@(C y x)
  | x == xlo || x == xhi
  , y == ylo || y == yhi = True
  | otherwise            = f a i
  where
    (C ylo xlo, C yhi xhi) = bounds a
