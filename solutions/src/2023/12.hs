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
      let dotCase  = answersA ! (groupI, springI + 1)
          hashCase = startGroup groupI (springI + 1) 
      in case arrIx springsA springI of
        Just '.' -> dotCase
        Just '#' -> hashCase
        Just '?' -> dotCase + hashCase
        Nothing | groupI == groupsN -> 1
        _                           -> 0

    startGroup groupI springI =
      case arrIx groupsA groupI of
        Just n  -> goGroup (groupI + 1) (n - 1) springI
        Nothing -> 0

    goGroup groupI n springI =
      let doneCase = answersA ! (groupI, springI + 1)
          moreCase = goGroup groupI (n-1) (springI + 1)
      in case arrIx springsA springI of
        Just '.' | n == 0    -> doneCase
        Just '#' | n >  0    -> moreCase
        Just '?' | n == 0    -> doneCase
                 | otherwise -> moreCase
        Nothing  | n == 0, groupI == groupsN -> 1
        _                                    -> 0
