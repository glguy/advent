{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/24>

-}
module Main where

import Advent.Format ( format )
import Data.List ( sortBy, sort )
import Data.Maybe ( listToMaybe )
import Data.Ord ( comparing )

data Packages = Packages { pkgSum, pkgCount, pkgProduct :: !Int }
  deriving (Eq, Show)

noPackages :: Packages
noPackages = Packages
  { pkgCount = 0
  , pkgSum = 0
  , pkgProduct = 1
  }

addPackage :: Int -> Packages -> Packages
addPackage p pkgs = Packages
  { pkgCount = pkgCount pkgs + 1
  , pkgSum   = pkgSum   pkgs + p
  , pkgProduct = fromIntegral p * pkgProduct pkgs
  }

instance Ord Packages where
  compare = comparing pkgCount <> comparing pkgProduct <> comparing pkgSum

search :: Int -> [Int] -> Maybe Int
search n ps0 = listToMaybe $
  do (pkg,ps1) <- sortBy (comparing fst) (start ps0)
     moreGroups (n-1) ps1
     return (pkgProduct pkg)
     
  where
  goal = sum ps0 `quot` n

  moreGroups 1 _ = [()]
  moreGroups i ps1 =
    do (_,ps2) <- start ps1
       moreGroups (i-1) ps2

  start = aux noPackages [] . sort

  aux :: Packages -> [Int] -> [Int] -> [(Packages,[Int])]
  aux a qs _ | pkgSum a == goal = [(a,qs)]
  aux _ _ [] = []
  aux a _ (p:_) | pkgSum (addPackage p a) > goal = []
  aux a qs (p:ps) = aux (addPackage p a) qs ps
                 ++ aux a (p:qs) ps

main :: IO ()
main =
  do input <- [format|24 (%u%n)*|]
     print (search 3 input)
     print (search 4 input)
