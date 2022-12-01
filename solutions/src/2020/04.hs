{-# Language BlockArguments, ScopedTypeVariables, QuasiQuotes, TemplateHaskell, ViewPatterns #-}
{-|
Module      : Main
Description : Day 4 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/4>

Passport validation

-}
module Main (main) where

import Advent (countBy, stageTH)
import Advent.Format (format)
import Data.Char (isDigit, isHexDigit)
import Data.List (delete, sort)

type Passport = [(F, String)]
data F = Fbyr | Fiyr | Feyr | Fhgt | Fhcl | Fecl | Fpid | Fcid deriving (Eq, Ord, Show)

stageTH

-- |
-- >>> :main
-- 245
-- 133
main :: IO ()
main =
  do inp <- [format|2020 4 (@F:%s( |%n))*&%n|]
     let xs = filter complete inp
     print (length xs)
     print (countBy (all (uncurry validate)) xs)

reqFields :: [F]
reqFields = sort [Fbyr, Fiyr, Feyr, Fhgt, Fhcl, Fecl, Fpid]

complete :: Passport -> Bool
complete x = reqFields == sort (delete Fcid (map fst x))

range :: Integer -> Integer -> Integer -> Bool
range lo hi x = lo <= x && x <= hi

validate :: F -> String -> Bool
validate Fbyr (reads -> [(n,""  )]) = range 1920 2002 n
validate Fiyr (reads -> [(n,""  )]) = range 2010 2020 n
validate Feyr (reads -> [(n,""  )]) = range 2020 2030 n
validate Fhgt (reads -> [(n,"cm")]) = range  150  193 n
validate Fhgt (reads -> [(n,"in")]) = range   59   76 n
validate Fhcl ('#':hcl)             = length hcl == 6 && all isHexDigit hcl
validate Fecl ecl                   = ecl `elem` words "amb blu brn gry grn hzl oth"
validate Fpid pid                   = length pid == 9 && all isDigit pid
validate Fcid _                     = True
validate _    _                     = False
