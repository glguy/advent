{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/10>

-}
module Main (main) where

import Advent (getInputLines, fromDigits)
import Data.Either (partitionEithers)
import Data.List (sort)

-- | >>> :main
-- 392043
-- 1605968119
main :: IO ()
main =
 do inp <- getInputLines 2021 10
    let (p1, p2) = partitionEithers (validate [] <$> inp)
    print (sum (map cost1 p1))
    print (median (map cost2 p2))

-- | Return the median of an odd-length list
--
-- >>> median [1,2,0]
-- 1
median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

-- | Either find the first unexpected bracket, or return the expected
-- sequence of closing characters.
--
-- >>> validate [] "{([(<{}[<>[]}>{[]{[(<()>"
-- Left '}'
--
-- >>> validate [] "[({(<(())[]>[[{[]{<()<>>"
-- Right "}}]])})]"
validate :: String -> String -> Either Char String
validate (x:xs) (y:ys) | x == y            = validate xs ys
validate xs     (y:ys) | Just x <- close y = validate (x:xs) ys
validate _      (y:_ )                     = Left y
validate xs     []                         = Right xs

-- | Return the character that closes this bracket, if there is one.
close :: Char -> Maybe Char
close '(' = Just ')'
close '[' = Just ']'
close '{' = Just '}'
close '<' = Just '>'
close _   = Nothing

cost1 :: Char -> Int
cost1 ')' = 3
cost1 ']' = 57
cost1 '}' = 1197
cost1 '>' = 25137
cost1 x   = error ("cost1: bad input " ++ show x)

-- | Compute the part 2 cost of the expected tail.
--
-- >>> cost2 "}}]])})]"
-- 288957
cost2 :: String -> Int
cost2 = fromDigits 5 . map cost2'

cost2' :: Char -> Int
cost2' ')' =  1
cost2' ']' =  2
cost2' '}' =  3
cost2' '>' =  4
cost2' x   = error ("cost2': bad input " ++ show x)
