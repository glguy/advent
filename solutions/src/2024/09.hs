{-# Language ImportQualifiedPost, LambdaCase #-}
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
import Data.Array.Unboxed (UArray, (!), accumArray, bounds)
import Data.Char (digitToInt)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.Maybe (listToMaybe)

-- | >>> :main
-- 6299243228569
-- 6326952672104
main :: IO ()
main =
 do [input] <- getInputLines 2024 9
    let digits = map digitToInt input
    let (files, free) = decFile [] [] 0 0 digits
    print (part1 (expandDiskArray (sum digits) files))
    print (part2 files free)

-- Input decoding --

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

-- | Expand a compressed disk into a run of file IDs and free space
-- markers using @-1@ for free space.
expandDiskArray :: Int -> [(Int, Int, Int)] -> UArray Int Int
expandDiskArray end files =
  accumArray (\_ x -> x) (-1) (0, end)
    [(offset, fileId) | (offset0, fileId, size) <- files
                      , offset <- [offset0 .. offset0 + size - 1]]

-- Part 1 --

-- | Compute the checksum resulting from defragmenting the expanded
-- disk according to the rules in part 1 where files can be split up
-- and compaction reads from the end.
part1 :: UArray Int Int -> Int
part1 a = uncurry (part1' a 0) (bounds a)

-- | Worker loop for 'part1' that tracks cursors for the next location to
-- checksum and the next available byte to be moved when free space is
-- encountered.
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

-- | Move all the files high-to-low to the lowest available contiguous
-- free block computing the checksum along the way.
part2 :: [(Int, Int, Int)] -> [(Int, Int)] -> Int
part2 files free = fst (foldl move1 (0, IntMap.fromList free) files)

-- | Given the file and free maps try to move the file to the lowest address
-- contiguous free block.
move1 :: (Int, IntMap Int) -> (Int, Int, Int) -> (Int, IntMap Int)
move1 (acc, free) (offset, fileId, fileSize) =
  let free1 = IntMap.takeWhileAntitone (< offset) free in -- discard out of range free blocks
  case pickFree fileSize free1 of
    Nothing         -> (acc + checksumOf offset fileId fileSize, free1)
    Just (o, free2) -> (acc + checksumOf o      fileId fileSize, free2)

-- | Find the first free region that can hold a file of the given size.
-- If one is identified remove it from the free list.
pickFree :: Int -> IntMap Int -> Maybe (Int, IntMap Int)
pickFree fileSize free = listToMaybe
  [ (offset, free2)
  | (offset, freeSize) <- IntMap.assocs free
  , freeSize >= fileSize
  , let free1 = IntMap.delete offset free
        free2 | freeSize == fileSize = free1
              | otherwise = IntMap.insert (offset + fileSize) (freeSize - fileSize) free1
  ]

-- | Compute the partial checksum for a file given: offset, file ID, file size
checksumOf :: Int -> Int -> Int -> Int
checksumOf offset fileId fileSize = fileId * (2 * offset + fileSize - 1) * fileSize `quot` 2
