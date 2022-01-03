{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/7>

-}
module Main where

import Advent
import Control.Monad
import Data.List

main :: IO ()
main =
  do xs <- [format|7 ((%a*)&(]|[)%n)*|]
     print (length (filter supportsTLS xs))
     print (length (filter supportsSSL xs))

split :: [String] -> ([String], [String])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:z) =
  case split z of
    (a,b) -> (x:a,y:b)

supportsTLS :: [String] -> Bool
supportsTLS xs =
  case split xs of
    (supers, hypers) -> any hasABBA supers && not (any hasABBA hypers)
  where
    hasABBA ys = any isABBA (tails ys)

    isABBA (w:x:y:z:_) = w == z && x == y && w /= x
    isABBA _ = False

supportsSSL :: [String] -> Bool
supportsSSL xs =
  case split xs of
    (supers, hypers) ->
      not $ null
       do s <- supers
          x:y:z:_ <- tails s
          guard (x == z && x /= y)
          h <- hypers
          guard ( [y,x,y] `isInfixOf` h )
