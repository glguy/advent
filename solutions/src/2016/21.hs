{-# Language QuasiQuotes, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/21>

-}
module Main where

import Control.Applicative ((<|>))
import Data.Char (isDigit)
import Data.List (elemIndex)
import Text.ParserCombinators.ReadP (ReadP, get, munch1)

import Advent (format)
data Scramble
  = RotateRight Int
  | RotateLeft Int
  | SwapPosition Int Int
  | SwapLetter Char Char
  | RotateChar Char
  | ReversePositions Int Int
  | MovePosition Int Int
  deriving Show

number :: ReadP Int
number = read <$> munch1 isDigit

s :: ReadP Scramble
s =
  RotateRight 1    <$ "rotate right 1 step"                 <|>
  RotateRight      <$ "rotate right "                       <*> number <* " steps" <|>
  RotateLeft 1     <$ "rotate left 1 step"                  <|>
  RotateLeft       <$ "rotate left "                        <*> number <* " steps" <|>
  SwapPosition     <$ "swap position "                      <*> number <* " with position " <*> number <|>
  SwapLetter       <$ "swap letter "                        <*> get    <* " with letter "   <*> get <|>
  RotateChar       <$ "rotate based on position of letter " <*> get <|>
  ReversePositions <$ "reverse positions "                  <*> number <* " through "       <*> number <|>
  MovePosition     <$ "move position "                      <*> number <* " to position "   <*> number

part1, part2 :: String
part1 = "abcdefgh"
part2 = "fbgdceah"

-- | >>> :main
-- dbfgaehc
-- aghfcdeb
main :: IO ()
main =
 do inp <- [format|2016 21 (@s%n)*|]
    putStrLn (foldl (flip forward) part1 inp)
    putStrLn (foldr backward part2 inp)

rotateRight :: Int -> [a] -> [a]
rotateRight n xs = b ++ a
  where
    n' = n `mod` length xs
    (a,b) = splitAt (length xs - n') xs

rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = b ++ a
  where
    n' = n `mod` length xs
    (a,b) = splitAt n' xs

set :: Int -> a -> [a] -> [a]
set i x xs = a ++ [x] ++ b
  where
    (a,_:b) = splitAt i xs

forward :: Scramble -> String -> String
forward scram =
  case scram of
    RotateRight i -> rotateRight i
    RotateLeft  i -> rotateLeft i
    SwapPosition i j -> \xs -> set i (xs!!j)
                             $ set j (xs!!i) xs
    SwapLetter x y -> map $ \a -> if a == x then y else if a == y then x else a
    RotateChar e -> rotatePositionOf e
    ReversePositions i j -> reverseRange i j
    MovePosition i j -> movePosition i j

backward :: Scramble -> String -> String
backward scram =
  case scram of
    RotateRight i -> rotateLeft i
    RotateLeft  i -> rotateRight i
    SwapPosition i j -> \xs -> set i (xs!!j)
                             $ set j (xs!!i) xs
    SwapLetter x y -> map $ \a -> if a == x then y else if a == y then x else a
    RotateChar e -> \xs ->
        case [a | i <- [0..length xs-1], let a = rotateRight i xs, rotatePositionOf e a == xs] of
          [x] -> x
          _   -> error "not unique"
    ReversePositions i j -> reverseRange i j
    MovePosition i j -> movePosition j i

rotatePositionOf :: Eq p => p -> [p] -> [p]
rotatePositionOf e xs = rotateRight j xs
  where
    Just i = elemIndex e xs
    j | i >=4     = i + 2
      | otherwise = i + 1

reverseRange :: Int -> Int -> [a] -> [a]
reverseRange i j xs = a ++ reverse c ++ d
  where
    (a,b) = splitAt i xs
    (c,d) = splitAt (j-i+1) b

movePosition :: Int -> Int -> [a] -> [a]
movePosition i j xs = c ++ [x] ++ d
  where
    (a,x:b) = splitAt i xs
    (c,d) = splitAt j (a++b)
