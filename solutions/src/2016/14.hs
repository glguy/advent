{-# Language ImportQualifiedPost, OverloadedStrings #-}
{-|
Module      : Main
Description : Day 14 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/14>

-}
module Main where

import Crypto.Hash.MD5 (hash)
import Data.ByteString.Builder (byteStringHex)
import Data.ByteString.Builder.Extra (toLazyByteStringWith, untrimmedStrategy)
import Data.List (isInfixOf, tails)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy qualified as L

myinp :: B.ByteString
myinp = "qzyelonm"

main :: IO ()
main =
  do print (solve 1)
     print (solve 2017)

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

seed :: Int -> B.ByteString
seed i = myinp <> B8.pack (show i)

solve :: Int -> Int
solve iterations =
  search (map (B8.unpack . iteratedHash iterations . seed) [0..]) !! 63

search :: [String] -> [Int]
search hashes =
  [ i | (i,h:hs) <- zip [0..] (tails hashes)
      , start <- take 1 [ x | x:y:z:_ <- tails h, x==y, y==z]
      , any (replicate 5 start `isInfixOf`) (take 1000 hs)
      ]
