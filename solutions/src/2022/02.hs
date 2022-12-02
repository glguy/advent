{-# Language QuasiQuotes, TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
{-|
Module      : Main
Description : Day 2 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/2>

-}
module Main where

import Advent (format, stageTH)

data A = AA | AB | AC deriving Show
data B = BX | BY | BZ deriving Show

stageTH

main :: IO ()
main =
 do input <- [format|2022 2 (@A @B%n)*|]
    print $ sum [outcome a b + shapeScore b | (a,b) <- input]
    print $ sum [outcome a b + shapeScore b | (a,b') <- input, let b = pick a b' ]

shapeScore :: B -> Int
shapeScore BX = 1
shapeScore BY = 2
shapeScore BZ = 3

outcome :: A -> B -> Int
outcome AA BX = 3
outcome AA BY = 6
outcome AA BZ = 0
outcome AB BX = 0
outcome AB BY = 3
outcome AB BZ = 6
outcome AC BX = 6
outcome AC BY = 0
outcome AC BZ = 3

desiredOutcome :: B -> Int
desiredOutcome BX = 0
desiredOutcome BY = 3
desiredOutcome BZ = 6

pick :: A -> B -> B
pick a b = head [ x | x <- [BX,BY,BZ], outcome a x == desiredOutcome b ]
