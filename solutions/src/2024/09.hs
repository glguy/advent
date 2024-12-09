{-# Language ImportQualifiedPost, LambdaCase, TransformListComp #-}
{-|
Module      : Main
Description : Day 9 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/9>

This solution processes a compressed disk representation, expands it into
file and free-space blocks, and computes a checksum based on defragmentation rules.

- Part 1: Files can be split and are compacted from the end of the disk.
- Part 2: Files cannot be split and are moved to the lowest contiguous free blocks.

>>> :main + "2333133121414131402"
1928
2858

-}
module Main (main) where

import Advent (getInputLines)
import Data.Array.Unboxed (UArray, (!), listArray)
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 6299243228569
-- 6326952672104
main :: IO ()
main =
 do [input] <- getInputLines 2024 9
    let digits = map digitToInt input
    print (part1 digits)
    print (part2 digits)

-- Part 1 --

-- | Expand a compressed disk into a run of file IDs and free space
-- markers using @-1@ for free space.
expand1 :: [Int] -> [Int]
expand1 = go1 0
  where
    go1 fileId = \case []   -> []
                       x:xs -> replicate x fileId ++ go2 (fileId + 1) xs
    go2 fileId = \case []   -> []
                       x:xs -> replicate x (-1)   ++ go1 fileId       xs

-- | Compute the checksum resulting from defragmenting the expanded
-- disk according to the rules in part 1 where files can be split up
-- and compaction reads from the end.
part1 :: [Int] -> Int
part1 encoded = part1' a 0 0 (n - 1)
  where
    xs = expand1 encoded
    n  = sum encoded
    a  = listArray (0, n - 1) xs

part1' ::
  UArray Int Int {- ^ offset to file ID -} ->
  Int            {- ^ partial checksum  -} ->
  Int            {- ^ left cursor       -} ->
  Int            {- ^ right cursor      -} ->
  Int            {- ^ complete checksum -}
part1' a acc i j
  | i > j      = acc
  | a ! i >= 0 = part1' a (acc + i * (a ! i)) (i + 1) j
  | a ! j >= 0 = part1' a (acc + i * (a ! j)) (i + 1) (j - 1)
  | otherwise  = part1' a acc i (j - 1)

-- Part 2 --

-- | Compute the checksum resulting from defragmenting the expanded
-- disk according to the rules in part 2 where files can't be split up
-- and compaction reads from the end and fills the earliest free block
-- with space available.
part2 :: [Int] -> Int
part2 input = moveAll files free
  where
    (files, free) = decFile [] [] 0 0 input

-- | Decode the input string where the first character is a file size.
decFile :: [(Int, Int, Int)] -> [(Int, Int)] -> Int -> Int -> [Int] -> ([(Int, Int, Int)], [(Int, Int)])
decFile files free nextId nextOff = \case
  x : xs -> decFree ((nextOff, nextId, x) : files) free (nextId + 1) (nextOff + x) xs
  [] -> (files, free)

-- | Decode the input string where the first character is a free block.
decFree :: [(Int, Int, Int)] -> [(Int, Int)] -> Int -> Int -> [Int] -> ([(Int, Int, Int)], [(Int, Int)])
decFree files free nextId nextOff = \case
  0 : xs -> decFile files free nextId nextOff xs
  x : xs -> decFile files ((nextOff, x) : free) nextId (nextOff + x) xs
  [] -> (files, free)

-- | Move all the files high-to-low to the lowest available contiguous
-- free block.
moveAll :: [(Int, Int, Int)] -> [(Int, Int)] -> Int
moveAll files free = fst (foldl' move1 (0, Map.fromList free) files)

-- | Given the file and free maps try to move the file to the lowest address
-- contiguous free block.
move1 :: (Int, Map Int Int) -> (Int, Int, Int) -> (Int, Map Int Int)
move1 (acc, free) (offset, fileId, fileSize)  =
  case [(k, v) | (k, v) <- Map.assocs free, then takeWhile by k < offset, v >= fileSize] of
    []         -> (acc + checksumOf offset fileId fileSize, free)
    (k, v) : _ -> (acc + checksumOf k      fileId fileSize, free')
      where
        free' | v == fileSize = Map.delete k free
              | otherwise     = Map.insert (k + fileSize) (v - fileSize) (Map.delete k free)

checksumOf :: Int -> Int -> Int -> Int
checksumOf offset fileId fileSize = fileId * (2 * offset + fileSize - 1) * fileSize `quot` 2
