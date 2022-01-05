{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/9>

-}
module Main where

import Advent ( getInputLines )
import Control.Applicative ( Alternative(some) )
import Text.ParserCombinators.ReadP as ReadP
import Data.Char (isDigit)

main :: IO ()
main =
  do xs <- getInputLines 2016 9
     print (sum (map decode1 xs))
     print (sum (map decode2 xs))

decode1 :: String -> Int
decode1 = mkDecode (\n xs -> n * length xs)

decode2 :: String -> Int
decode2 = mkDecode (\n xs -> n * decode2 xs)

mkDecode ::
  (Int -> String -> Int) {- ^ repeated segment logic -} ->
  String                 {- ^ input string           -} ->
  Int                    {- ^ decoded length         -}
mkDecode f = fst . head . readP_to_S (sum <$> ReadP.many (repeated <++ plain) <* eof)
  where
    number = read <$> munch1 isDigit
    plain = length <$> munch1 (/='(')
    repeated =
       do len <- char '(' *> number <* char 'x'
          f <$> number <* char ')' <*> ReadP.count len get
