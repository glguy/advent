{-# Language BlockArguments, QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 13 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/13>

-}
module Main (main) where

import Advent.Format (format)
import Data.List (foldl')
import Data.Set qualified as Set
import Intcode

-- | >>> :main
-- 462
-- 23981
main :: IO ()
main =
  do mach <- new <$> [format|2019 13 %d&,%n|]

     print (part1 mach)
     print (robot Nothing Nothing 0 (run (set 0 2 mach)))

part1 :: Machine -> Int
part1 = Set.size . foldl' write Set.empty . tileWrites . run
  where
    write blocks (x,y,2) = Set.insert (x,y) blocks
    write blocks (x,y,_) = Set.delete (x,y) blocks

tileWrites :: Effect -> [(Int, Int, Int)]
tileWrites effect =
  case effect of
    Halt                                   -> []
    Output x (Output y (Output t effect')) -> (x,y,t) : tileWrites effect'
    _                                      -> error "tileWrites: bad program"

robot :: Maybe Int -> Maybe Int -> Int -> Effect -> Int
robot ball paddle score effect =
  case effect of

    Halt -> score

    Output (-1) (Output 0 (Output score' effect')) ->
      robot ball paddle score' effect'

    Output x (Output _ (Output t effect'))
      | t == 3 -> robot ball (Just x) score effect'
      | t == 4 -> robot (Just x) paddle score effect'
      | otherwise -> robot ball paddle score effect'

    Input f ->
        robot ball paddle score
         case (ball, paddle) of
           (Just b, Just p)
             | b < p -> f (-1)
             | b > p -> f 1
           _ -> f 0

    _ -> error "robot: bad program"
