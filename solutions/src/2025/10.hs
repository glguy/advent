{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/10>

>>> :{
:main +
"[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"
:}
7
33

-}
module Main (main) where

import Advent (format)
import Data.Bits (xor, bit)
import Data.Foldable (for_)
import Data.List (elemIndices)
import Data.Maybe (catMaybes)
import Data.SBV (SInteger, optLexicographic, free, minimize, (.==), (.>=), constrain, getModelValue)
import Data.Traversable (for)

-- | >>> :main
-- 409
-- 15489
main :: IO ()
main =
 do input <- [format|2025 10 ([%c*]( %(%d&,%))* {%d&,}%n)*|]
    print (sum (map part1 input))
    xs <- traverse part2 input
    print (sum xs)

part1 :: ([Char], [[Int]], [Int]) -> Int
part1 (goal, btns, _) =
  minimum [length xs | xs <- cases, xorBits (concat xs) == target]  
  where
    pickOrNot x = [Nothing, Just x]
    cases = catMaybes <$> traverse pickOrNot btns
    target = xorBits (elemIndices '#' goal)

-- | Construct the integers by xoring together the bit given by index.
xorBits :: [Int] -> Integer
xorBits = foldl (\acc i -> acc `xor` bit i) 0

part2 :: ([Char], [[Int]], [Int]) -> IO Integer
part2 (_, btns, jolt) =
  do
    res <- optLexicographic
      do -- allocate one, non-zero coefficient per button to press
        cs <- for [0 .. length btns - 1] \i ->
          do
            c <- free ("x" ++ show i)
            constrain (c .>= 0)
            pure (c :: SInteger)

        -- add a constraint for each element of the joltage
        for_ (zip [0 .. ] jolt) \(i, j) ->
          let j' = sum [c | (c, btn) <- zip cs btns, i `elem` btn]
          in constrain (j' .== fromIntegral j)

        -- optimize the problem for the smallest number of button presses
        minimize "smallest sum" (sum cs)

    case getModelValue "smallest sum" res of
      Just x -> pure x
      Nothing -> fail "no solution"
