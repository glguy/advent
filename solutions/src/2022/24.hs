{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/24>

>>> :{
:main +
  "#.######\n\
  \#>>.<^<#\n\
  \#.<..<<#\n\
  \#>v.><>#\n\
  \#<^v^^>#\n\
  \######.#\n"
:}
18
54

-}
module Main where

import Data.Ix (inRange)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Advent ( getInputMap )
import Advent.Coord ( cardinal, west, Coord(..) )

data World = World {
  stormsE, stormsW, stormsN, stormsS :: Map Int (Set Int),
  region    :: Coord 
} deriving (Show)

parseWorld :: Map Coord Char -> World
parseWorld m = World {
  stormsE = Map.fromListWith Set.union [(y-1, Set.singleton (x-1)) | (C y x, '>') <- Map.assocs m],
  stormsW = Map.fromListWith Set.union [(y-1, Set.singleton (x-1)) | (C y x, '<') <- Map.assocs m],
  stormsN = Map.fromListWith Set.union [(x-1, Set.singleton (y-1)) | (C y x, '^') <- Map.assocs m],
  stormsS = Map.fromListWith Set.union [(x-1, Set.singleton (y-1)) | (C y x, 'v') <- Map.assocs m],
  region = maximum (Map.keys m)
}

checkBlizzard :: World -> Int -> Coord -> Bool
checkBlizzard w t (C y x) =
  seq w $
  maybe False (Set.member ((x'-t)`mod`(xm-1))) (Map.lookup y' (stormsE w)) ||
  maybe False (Set.member ((x'+t)`mod`(xm-1))) (Map.lookup y' (stormsW w)) ||
  maybe False (Set.member ((y'-t)`mod`(ym-1))) (Map.lookup x' (stormsS w)) ||
  maybe False (Set.member ((y'+t)`mod`(ym-1))) (Map.lookup x' (stormsN w))
  where
    y' = y-1
    x' = x-1
    C ym xm = region w

-- |
-- >>> :main
-- 295
-- 851
main :: IO ()
main =
 do input <- Map.filter ('.' /=) <$> getInputMap 2022 24
    let w = parseWorld input
    let corner = maximum (Map.keys input)
    let target = corner + west

    let loop t end prev
          | Set.member end prev = t
          | otherwise = loop (t+1) end (grow w (t+1) end prev)

    let t1 = loop 0  target  (Set.singleton (C 0 1))
        t2 = loop t1 (C 0 1) (Set.singleton target)
        t3 = loop t2 target  (Set.singleton (C 0 1))
    print t1
    print t3

-- | Given a set of locations the elf could be find the set the elf can be at next.
grow :: World -> Int -> Coord -> Set Coord -> Set Coord
grow w t end prev =
  Set.fromList
    [ next
      | here <- Set.toList prev
      , next <- here : cardinal here
      , inRange (1,region w-1) next || next == here || next == end
      , not (checkBlizzard w t next)
    ]
