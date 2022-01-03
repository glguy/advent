{-# Language ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/19>

-}
module Main where

import Data.Sequence (Seq)
import Data.Sequence qualified as Seq

elves :: Int
elves = 3012210

main :: IO ()
main =
  do print (part1 [1..elves])
     print (part2 [1..elves])

part1 :: [a] -> Maybe a
part1 = part1' []

part1' :: [a] -> [a] -> Maybe a
part1' [] []       = Nothing
part1' [] [x]      = Just x
part1' ys (x:_:xs) = part1' (x:ys) xs
part1' ys xs       = part1' [] (xs ++ reverse ys)

part2 :: [a] -> Maybe a
part2 = part2' . Seq.fromList

part2' :: Seq a -> Maybe a
part2' xs =
  case Seq.viewl xs of
    Seq.EmptyL -> Nothing
    x Seq.:< ys
      | Seq.null ys -> Just x
      | otherwise   -> part2' ((l Seq.>< Seq.drop 1 r) Seq.|> x)
          where (l,r) = Seq.splitAt (half (length ys)) ys
                half x = (x-1) `div` 2
