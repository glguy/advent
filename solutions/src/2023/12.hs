{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/12>

>>> :{
:main +
"???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"
:}
21
525152

-}
module Main (main) where

import Advent (format, arrIx)
import Data.Array (Ix(range), (!), listArray)
import Data.List (intercalate)

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

    go groupI springI =
      case arrIx springsA springI of
        Just '.' -> answersA ! (groupI, springI + 1)
        Just '#' -> startGroup groupI (springI + 1)
        Just '?' -> startGroup groupI (springI + 1)
                  + answersA ! (groupI, springI + 1)
        _ | groupI == groupsN -> 1
          | otherwise         -> 0

    startGroup groupI springI =
      case arrIx groupsA groupI of
        Nothing -> 0
        Just n -> goGroup (groupI + 1) (n - 1) springI

    goGroup groupI n springI =
      case arrIx springsA springI of
        Nothing  | n == 0, groupI == groupsN -> 1
        Just '.' | n == 0 -> answersA ! (groupI, springI + 1)
        Just '?' | n == 0 -> answersA ! (groupI, springI + 1)
                 | otherwise -> goGroup groupI (n-1) (springI + 1)
        Just '#' | n > 0     -> goGroup groupI (n-1) (springI + 1)
        _ -> 0
