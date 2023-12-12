{-# Language QuasiQuotes #-}
{-# OPTIONS_GHC -w #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/12>

-}
module Main where

import Advent (format)
import Data.List (intercalate)
import Data.Array

-- |
--
-- >>> :main
-- 6871
-- 2043098029844
main :: IO ()
main =
 do input <- [format|2023 12 (%s %d&,%n)*|]
    print (sum [ways g s | (s,g) <- input])
    print (sum [ways (concat (replicate 5 g)) (unfoldSprings s) | (s,g) <- input])

unfoldSprings :: String -> String
unfoldSprings = intercalate "?" . replicate 5

ways :: [Int] -> [Char] -> Int
ways groups springs = answersA ! (0,0)
  where
    groupsN = length groups
    groupsA = listArray (0, groupsN - 1) groups

    springsN = length springs
    springsA = listArray (0, springsN - 1) springs

    answersB = ((0,0),(groupsN,springsN))
    answersA = listArray answersB [go i j | (i,j) <- range answersB]

    go groupI springI
      | groupI == groupsN =
        if all (\i -> springsA ! i `elem` ".?") [springI .. springsN - 1]
          then 1 else 0

      | springI == springsN = 0

      | otherwise =
        case springsA ! springI of
          '.' -> answersA ! (groupI, springI + 1)
          '#' -> startGroup (groupI + 1) ((groupsA ! groupI) - 1) (springI + 1)
          '?' -> startGroup (groupI + 1) ((groupsA ! groupI) - 1) (springI + 1)
               + answersA ! (groupI, springI + 1)
          _ -> error "bad diagram"

    startGroup groupI n springI
      | springI == springsN = if n == 0 && groupI == groupsN then 1 else 0
      | n == 0, springsA ! springI `elem` ".?" = answersA ! (groupI, springI + 1)
      | n == 0 = 0
      | '.' == springsA ! springI = 0
      | otherwise = startGroup groupI (n-1) (springI + 1)
