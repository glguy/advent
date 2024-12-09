{-# Language QuasiQuotes, ImportQualifiedPost, LambdaCase #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/9>

>>> :main + "2333133121414131402"
1928
2858

-}
module Main (main) where

import Advent (getInputLines)
import Data.Char (digitToInt)
import Data.Map qualified as Map
import Data.Map (Map)

-- | >>> :main
-- 6299243228569
-- 6326952672104
main :: IO ()
main =
 do [input] <- getInputLines 2024 9
    let (files, free) = decFile Map.empty Map.empty 0 0 (map digitToInt input)
    print (checksum (comp1 files free))
    print (checksum (comp2 files free))

-- | Decode the input string where the first character is a file size.
decFile :: Map Int (Int, Int) -> Map Int Int -> Int -> Int -> [Int] -> (Map Int (Int, Int), Map Int Int)
decFile files free nextId nextOff = \case
  x : xs -> decFree (Map.insert nextOff (nextId, x) files) free (nextId + 1) (nextOff + x) xs
  [] -> (files, free)

-- | Decode the input string where the first character is a free block.
decFree :: Map Int (Int, Int) -> Map Int Int -> Int -> Int -> [Int] -> (Map Int (Int, Int), Map Int Int)
decFree files free nextId nextOff = \case
  0 : xs -> decFile files free nextId nextOff xs
  x : xs -> decFile files (Map.insert nextOff x free) nextId (nextOff + x) xs
  [] -> (files, free)

-- | Compute the checksum of a filesystem.
checksum :: Map Int (Int, Int) -> Int
checksum files =
   sum [k * fileId | (offset, (fileId, fileSize)) <- Map.assocs files
                   , k <- take fileSize [offset ..]]

-- | Move all the bytes of the files from the high offsets down
-- to the lowest free addresses. Files are moved from the highest
-- offsets to the lowest free offsets. Files can be fragmented.
comp1 :: Map Int (Int, Int) -> Map Int Int -> Map Int (Int, Int)
comp1 files free =
   case (Map.minViewWithKey free, Map.maxViewWithKey files) of
      (Just ((k1,v1), free'), Just ((k2, (i, s)), files'))
        | k1 < k2
        , s == v1 -> comp1 (Map.insert k1 (i, s) files') free'
        | k1 < k2
        , s < v1 -> comp1 (Map.insert k1 (i, s) files')
                          (Map.insert (k1+s) (v1-s) free')
        | k1 < k2
        , s > v1 -> comp1 (Map.insert k1 (i, v1)
                           (Map.insert k2 (i, s-v1) files'))
                           free'
      _ -> files

-- | Move all the files high-to-low to the lowest available contiguous
-- free block.
comp2 :: Map Int (Int, Int) -> Map Int Int -> Map Int (Int, Int)
comp2 files free = fst (foldl' (uncurry move) (files, free) (reverse (Map.keys files)))

-- | Given the file and free maps try to move the file at the given
-- offset to the lowest address contiguous free block.
move :: Map Int (Int, Int) -> Map Int Int -> Int -> (Map Int (Int, Int), Map Int Int)
move files free offset =
   let (fileId, fileSize) = files Map.! offset in
   case [(k, v) | (k, v) <- Map.assocs free, k < offset, v >= fileSize] of
      [] -> (files, free)
      (k, v) : _ -> (Map.insert k (fileId, fileSize) (Map.delete offset files), free2)
         where
            free1 = Map.delete k free
            free2 | v == fileSize = free1
                  | otherwise = Map.insert (k + fileSize) (v - fileSize) free1