{-# LANGUAGE ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/16>

-}
module Main where

import Advent (format)
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vector

toBool :: Char -> Bool
toBool x = x == '1'

fromBool :: Bool -> Char
fromBool x = if x then '1' else '0'

part1, part2 :: Int
part1 = 272
part2 = 35651584

expand :: Int -> Vector Bool -> Vector Bool
expand n seed
  | Vector.length seed >= n = Vector.take n seed
  | otherwise = expand n
              $ seed <> Vector.singleton False <>
                Vector.map not (Vector.reverse seed)

checksum :: Vector Bool -> [Char]
checksum v
  | odd n     = fromBool <$> Vector.toList v
  | otherwise = checksum
              $ Vector.generate (n`quot`2) $ \i ->
                   v Vector.! (2*i) == v Vector.! (2*i+1)
  where
    n = Vector.length v

-- | >>> :main
-- 11111000111110000
-- 10111100110110100
main :: IO ()
main =
 do input <- [format|2016 16 (0|1)*!%n|]
    let v = Vector.fromList (toBool <$> input)
    putStrLn (checksum (expand part1 v))
    putStrLn (checksum (expand part2 v))
