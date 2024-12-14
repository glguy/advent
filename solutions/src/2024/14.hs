{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/14>

To find the answer to part 2 I tried a bunch of stuff.
What actually worked was finding pictures with many adjacent
pixels. Since this is just a wild guess I didn't bother
committing it into the repo.

-}
module Main where

import Advent (counts, format)
import Advent.Coord (Coord(C), drawCoords, zipCoord)

room :: Coord
room = C 103 101

main :: IO ()
main =
 do input <- [format|2024 14 (p=%d,%d v=%d,%d%n)*|]
    print (product (counts (concatMap (toQuad . step 100) input)))
    putStrLn (drawCoords (map (step 7051) input))

toQuad :: Coord -> [Int]
toQuad (C y x)
  | x < roomX `div` 2, y < roomY `div` 2 = [1]
  | x > roomX `div` 2, y < roomY `div` 2 = [2]
  | x < roomX `div` 2, y > roomY `div` 2 = [3]
  | x > roomX `div` 2, y > roomY `div` 2 = [4]
  | otherwise = []
  where
    C roomY roomX = room

step :: Int -> (Int, Int, Int, Int) -> Coord
step n (x, y, dx, dy) = zipCoord mod (C (y + n * dy) (x + n * dx)) room
