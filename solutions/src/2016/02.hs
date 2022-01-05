{-# Language TemplateHaskell, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/2>

-}
module Main where

import Advent (arrIx, format)
import Advent.Coord (Coord(..), east, north, origin, south, west)
import Data.Foldable (foldl')
import Data.Array (Array, (!), listArray)

data D = DL | DR | DU | DD

mempty

-- | >>> :main
-- 97289
-- 9A7DC
main :: IO ()
main =
 do cmds <- [format|2016 2 (@D*%n)*|]
    putStrLn (computeCode keys1 cmds)
    putStrLn (computeCode keys2 cmds)

keys1 :: Array Coord Char
keys1 = listArray (C (-1) (-1), C 1 1)
  "123\
  \456\
  \789"

keys2 :: Array Coord Char
keys2 = listArray (C (-2) (-2), C 2 2)
  "..1..\
  \.234.\
  \56789\
  \.ABC.\
  \..D.."

computeCode :: Array Coord Char -> [[D]] -> String
computeCode ks cmds = map (ks!) (tail (scanl (process ks) origin cmds))

process ::
  Array Coord Char {- ^ key pad           -} ->
  Coord            {- ^ starting position -} ->
  [D]              {- ^ command           -} ->
  Coord            {- ^ stopping position -}
process ks = foldl' aux
  where
    aux pos mov
      | isValid ks pos' = pos'
      | otherwise       = pos
      where
        pos' = pos + translate mov

isValid :: Array Coord Char -> Coord -> Bool
isValid ks i = maybe False (/= '.') (arrIx ks i)

translate :: D -> Coord
translate DL = west
translate DR = east
translate DU = north
translate DD = south
