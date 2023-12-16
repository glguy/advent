{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 12 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/12>

This problem asks us to find the number of unqiue
rows that satisfy the grouping constraint. The question
mark characters are wildcards.

A naive enumeration solution won't work here, there are
far too many possible assignments in part 2. This solution
uses a boxed array to implement a dynamic programing solution
to the problem.

Because the array is boxed we can lean on laziness to resolve
all of the data dependencies entailed by the dynamic programming
approach implicitly. By indexing on Ints representing the
suffix instead of suffixes as Map keys we get a performance
speedup.

To break the problem into increasingly smaller components
we solve it for all the suffixes of the input pattern and
group constraint.

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
import Data.Array (range, (!), listArray)
import Data.List (intercalate)

-- | Parse the input sequences and print out answers to both parts.
--
-- >>> :main
-- 6871
-- 2043098029844
main :: IO ()
main =
 do input <- [format|2023 12 (%s %d&,%n)*|]
    print (sum [ways g s | (s,g) <- input])
    print (sum [ways (concat (replicate 5 g)) (unfoldSprings s) | (s,g) <- input])

-- | Expand the input row as specified for part 2
--
-- >>> unfoldSprings ".#"
-- ".#?.#?.#?.#?.#"
unfoldSprings :: String -> String
unfoldSprings = intercalate "?" . replicate 5

-- | Given a group clue and an spring sequence, compute the number
-- of unique rows that match the clue.
--
-- >>> ways [3,2,1] "?###????????"
-- 10
ways :: [Int] -> String -> Int
ways groups springs = answersA ! (0,0)
  where
    groupsN = length groups
    groupsA = listArray (0, groupsN - 1) groups

    springsN = length springs
    springsA = listArray (0, springsN - 1) springs

    answersB = ((0, 0), (groupsN, springsN))
    answersA = listArray answersB [go i j | (i,j) <- range answersB]

    -- recusive calls to go are memoized via the array
    rec groupI springI = answersA ! (groupI, springI)

    -- compute the number of matches at suffixes starting at these indexes
    go groupI springI =
      let dotCase  = rec groupI (springI + 1)
          hashCase = startGroup groupI (springI + 1)
          {-# Inline hashCase #-} in -- improved benchmark results
      case arrIx springsA springI of
        Just '.'   -> dotCase
        Just '#'   -> hashCase
        Just '?'   -> hashCase + dotCase
        _          -> if groupI == groupsN then 1 else 0

    -- compute the number of ways assuming the next group starts here
    startGroup groupI springI =
      case arrIx groupsA groupI of
        Just n     -> loopGroup (groupI + 1) springI (n - 1)
        Nothing    -> 0 -- no group available to start

    loopGroup groupI springI 0 = -- end of group
      case arrIx springsA springI of
        Nothing    -> if groupI == groupsN then 1 else 0
        Just '#'   -> 0 -- group too long
        _          -> rec groupI (springI + 1)

    loopGroup groupI springI n = -- middle of group
      case arrIx springsA springI of
        Just '.'   -> 0 -- group too short
        Nothing    -> 0 -- group too short
        _          -> loopGroup groupI (springI + 1) (n - 1)
