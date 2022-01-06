{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/14>

-}
module Main where

import Advent (format)
import Crypto.Hash.MD5 (hash)
import Data.ByteString qualified as B
import Data.ByteString.Builder (byteStringHex)
import Data.ByteString.Builder.Extra (toLazyByteStringWith, untrimmedStrategy)
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as L
import Data.List (isInfixOf, tails)

-- | >>> :main
-- 15168
-- 20864
main :: IO ()
main =
  do input <- [format|2016 14 %s%n|]
     print (solve input 1)
     print (solve input 2017)

-- | Hash a bytestring to to ASCII encoded, lowercase hex
hashmd5 :: B.ByteString -> B.ByteString
hashmd5
  = L.toStrict
  . toLazyByteStringWith md5strategy L.empty
  . byteStringHex
  . hash
  where
    md5strategy = untrimmedStrategy 32 32

iteratedHash :: Int -> B.ByteString -> B.ByteString
iteratedHash n x
  | n <= 0 = x
  | otherwise = iteratedHash (n-1) (hashmd5 x)

seed :: String -> Int -> B.ByteString
seed input i = B8.pack (input ++ show i)

solve :: String -> Int -> Int
solve input iterations =
  search (map (B8.unpack . iteratedHash iterations . seed input) [0..]) !! 63

search :: [String] -> [Int]
search hashes =
  [ i | (i,h:hs) <- zip [0..] (tails hashes)
      , start <- take 1 [ x | x:y:z:_ <- tails h, x==y, y==z]
      , any (replicate 5 start `isInfixOf`) (take 1000 hs)
      ]
