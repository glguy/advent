{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/10>

-}
module Main where

import Advent (format)
import Data.Bits (xor, bit)
import Data.Foldable (for_)
import Data.SBV (optLexicographic, free, minimize, (.==), (.>=), constrain, literal, getModelValue)

main :: IO ()
main =
 do input <- [format|2025 10 ([%c*]( %(%d&,%))* {%d&,}%n)*|]
    print (sum (map part1 input))
    xs <- traverse part2 input
    print (sum xs)

part1 :: ([Char], [[Int]], [Int]) -> Int
part1 (goal, btns, jolt) = minimum (map (length . filter (not . null)) valid)
  where
    target = sum [bit i | (i, '#') <- zip [0..] goal]
    valid =
      filter (\xs -> decode (concat xs) == target)
        (traverse (\btn -> [[], btn]) btns)

decode :: [Int] -> Int
decode = foldl (\acc i -> acc `xor` bit i) 0

part2 :: ([Char], [[Int]], [Int]) -> IO Integer
part2 (_, btns, jolt) = do
  res <- optLexicographic
     do cs <- traverse free ["x" ++ show i | i <- [0 .. length btns - 1]]
        for_ cs \c -> constrain (c .>= 0)
        for_ (zip [0..] jolt) \(i, j) ->
          constrain
            (sum [c | (c, btn) <- zip cs btns, i `elem` btn]
             .== literal (fromIntegral j :: Integer))
        minimize "smallest sum" (sum cs)
  case getModelValue "smallest sum" res of
    Just x -> pure x
    Nothing -> fail "no solution"
