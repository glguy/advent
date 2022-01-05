{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-# Options_GHC -w #-}
{-|
Module      : Main
Description : Day 16 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/16>

-}
module Main (main) where

import Advent
import Advent.Format (format)
import Control.Applicative (some)
import Data.List ((\\), isPrefixOf, sortOn, transpose)
import Data.Set qualified as Set
import Data.Map qualified as Map

type Range = (Integer, Integer)
type Field = ([String], [Range])

match1 :: Integer -> Range -> Bool
match1 x (lo,hi) = lo <= x && x <= hi

match :: Field -> Integer -> Bool
match (_,range) x = any (match1 x) range

------------------------------------------------------------------------

-- |
-- >>> :main
-- 25916
-- 2564529489989
main :: IO ()
main =
  do (fields, yourTicket, nearbyTickets) <-
       [format|2020 16
         (%s& : (%lu-%lu)&( or )%n)*%n
         your ticket:%n
         (%lu&,)%n
         %n
         nearby tickets:%n
         (%lu&,%n)*
       |]

     -- print sum of invalid fields
     print $ sum [x | xs <- nearbyTickets, x <- xs, not (any (`match` x) fields)]

     let goodTickets = [xs | xs <- nearbyTickets, all (\x -> any (`match` x) fields) xs]

         possibleFields col = Set.fromList [fst field | field <- fields, all (match field) col]

         allCandidates = [possibleFields col | col <- transpose goodTickets]

         -- pair up my ticket's field values with the candidate field names
         constraints = Map.fromList (zip yourTicket allCandidates)

     print $ product [i | (i, name) <- Map.toList (head (uniqueAssignment constraints))
                        , ["departure"] `isPrefixOf` name]
