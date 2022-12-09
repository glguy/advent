{-# Language QuasiQuotes, TemplateHaskell #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/9>

-}
module Main where

import Advent ( format, ordNub, stageTH )
import Advent.Coord ( above, below, left, origin, right, Coord(C) )

data C = CD | CR | CU | CL deriving Show

stageTH

-- |
-- >>> :main
-- 5930
-- 2443
main :: IO ()
main = do
    input <- [format|2022 9 (@C %u%n)*|]
    let fullInput = concatMap expand input
    let headPath = scanl drive origin fullInput
    let tailPaths = iterate (scanl updateTail origin) headPath
    print (length (ordNub (tailPaths !! 1)))
    print (length (ordNub (tailPaths !! 9)))

expand :: (a, Int) -> [a]
expand (x,n) = replicate n x

drive :: Coord -> C -> Coord
drive here move =
    case move of
        CD -> below here
        CU -> above here
        CL -> left  here
        CR -> right here

updateTail ::
  Coord {- ^ tail -} ->
  Coord {- ^ head -} ->
  Coord
updateTail t@(C ty tx) (C hy hx)
  | touching ty hy, touching tx hx = t
  | otherwise = C (closer ty hy) (closer tx hx)

touching :: Int -> Int -> Bool
touching x y = abs (x - y) < 2

closer :: Int -> Int -> Int
closer ty hy = signum (hy - ty) + ty
