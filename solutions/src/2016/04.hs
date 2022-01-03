{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/4>

-}
module Main where

import Advent (format, counts)
import Data.Char (ord, chr)
import Data.List (sortBy)
import qualified Data.Map as Map

type Entry = ([String], Int, String)

-- | >>> :main
-- 158835
-- [993]
main :: IO ()
main =
 do input <- [format|4 ((%a+-)*%d[%a*]%n)*|]
    let valid = [e | e <- input, isGoodEntry e]
    print (sum [sid | (_, sid, _) <- valid])
    print [sid | e@(_, sid, _) <- valid, decryptEntry e == "northpole object storage"]

decryptEntry :: Entry -> String
decryptEntry (name, sid, _) = unwords (map (map (decrypt sid)) name)

isGoodEntry :: Entry -> Bool
isGoodEntry (name, _, hash) = hash == computeHash (concat name)

computeHash :: String -> String
computeHash x = take 5 (map fst (sortBy ordering (Map.toList (counts x))))
  where
    ordering (xa,xn) (ya,yn)
       = compare yn xn -- descending
      <> compare xa ya -- ascending

decrypt :: Int -> Char -> Char
decrypt n c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')
