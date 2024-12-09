{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/9>

-}
module Main (main) where

import Advent
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Set qualified as Set
import Data.Set (Set)

import Data.List
import Data.Char

-- | >>> :main
main :: IO ()
main =
 do input <- [format|2024 9 %s%n|]
    let (stuff, free) = decodeFile Map.empty Map.empty 0 0 (map digitToInt input)
    print $ sum [x*y | (x,y) <- Map.assocs (compact stuff 0)]
    print $ sum [x*y | (x,y) <- Map.assocs (compact2 stuff free)]

decodeFile disk free fileid off (x:xs) =
  let disk' = foldl' (\acc i -> Map.insert i fileid acc) disk (take x [off ..])
  in decodeFree disk' free (fileid+1) (off+x) xs
decodeFile disk free _ _ [] = (disk, free)

decodeFree disk free fileid off (x:xs) =
   let free' = Map.insert off x free
   in decodeFile disk free' fileid (off+x) xs
decodeFree disk free _ _ [] = (disk, free)

compact disk i =
   case Map.lookup i disk of
      Nothing ->
         case Map.maxViewWithKey disk of
            Just ((mk, kv), disk') ->
               if mk > i then
                  compact (Map.insert i kv disk') (i+1)
               else
                  disk
      Just{} -> compact disk (i+1)

compact2 disk free = step disk free (maximum disk)

step disk free (-1) = disk  
step disk free fileid =
   case [(off,sz) | (off, sz) <- Map.assocs free, sz >= length keys] of
     [] -> step disk free (fileid-1)
     (off,sz):_ | off < minimum keys ->
            let diskR = foldl' (\acc i -> Map.delete i acc) disk keys
                diskS = foldl' (\acc i -> Map.insert i fileid acc) diskR (take n [off ..])
                free' | sz == n = Map.delete off free
                      | otherwise = Map.insert (off+n) (sz-n) (Map.delete off free)
            in step diskS free' (fileid-1)
        | otherwise -> step disk free (fileid-1)
   where
      keys = [k | (k,i) <- Map.assocs disk, i==fileid]
      n = length keys
