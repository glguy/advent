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

import Advent (counts, format, times)
import Advent.Coord (Coord(C), drawCoords, zipCoord)

room :: Coord
room = C 103 101

data Bot = Bot !Coord !Coord

main :: IO ()
main =
 do input <- [format|2024 14 (p=%d,%d v=%d,%d%n)*|]
    let bots = [Bot (C y x) (C dy dx) | (x, y, dx, dy) <- input]
    print (product (counts (concatMap (toQuad . times 100 step) bots)))
    putStrLn (drawCoords [p | Bot p _ <- map (times 7051 step) bots])

toQuad :: Bot -> [Int]
toQuad (Bot (C y x) _)
  | x < roomX `div` 2, y < roomY `div` 2 = [1]
  | x > roomX `div` 2, y < roomY `div` 2 = [2]
  | x < roomX `div` 2, y > roomY `div` 2 = [3]
  | x > roomX `div` 2, y > roomY `div` 2 = [4]
  | otherwise = []
  where
    C roomY roomX = room

step :: Bot -> Bot
step (Bot p v) = Bot (zipCoord mod (p + v) room) v
