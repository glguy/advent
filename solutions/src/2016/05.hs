{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/5>

Search for a password that satisfies a leading zeros MD5 property.

-}
module Main where

import Advent.Format (format)
import Crypto.Hash.MD5 (hash)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as B
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Text.Printf (printf)

main :: IO ()
main =
 do input <- B.pack <$> [format|2016 05 %s%n|]
    hSetBuffering stdout NoBuffering
    putStrLn (password1 input)
    putStrLn (password2 input)

passwordLen :: Int
passwordLen = 8

password1 :: B.ByteString -> String
password1 input = fst <$> take passwordLen (digitStream input)

password2 :: B.ByteString -> String
password2 input = replicate passwordLen '_'
         ++ go 0 IntMap.empty digitStream'
  where
    digitStream' =
        [ (key, val) | (pos,val) <- digitStream input
        , let key = fromEnum pos - fromEnum '0'
        , 0 <= key, key < passwordLen
        ]

    go _ seen _ | length seen == passwordLen = ""
    go _ _ [] = error "password generation underflow!"
    go n seen ((key,val) : rest)
      | IntMap.member key seen = render n seen ++ go (n+1) seen rest
      | otherwise              = render n seen' ++ go (n+1) seen' rest
      where
        seen' = IntMap.insert key val seen

spinner :: String
spinner    = "◐◓◑◒"

spinnerLen :: Int
spinnerLen = length spinner

render :: Int -> IntMap Char -> String
render n seen =
  '\r' : (spinner !! (n`rem`spinnerLen)) : ' ' :
  [ IntMap.findWithDefault '_' key seen | key <- [0 .. passwordLen - 1 ] ]

hexRep :: BS.ByteString -> String
hexRep bs = printf "%02x" =<< BS.unpack bs

digitStream :: B.ByteString -> [(Char,Char)]
digitStream input = go (0 :: Int)
  where
    go i =
      case splitAt 5 (hexRep (hash (input <> B.pack (show i)))) of
        ("00000",c1:c2:_) -> (c1,c2) : go (i+1)
        _ -> go (i+1)
