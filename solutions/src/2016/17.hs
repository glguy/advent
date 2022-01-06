{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 17 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2016/day/17>

-}
module Main where

import Advent (format)
import Advent.Coord (east, north, origin, south, west, Coord(..))
import Advent.Search (bfs)
import Crypto.Hash.MD5 (hash)
import Data.Bits ((.&.), shiftR)
import Data.ByteString qualified as BS
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B8

-- | >>> :main
-- DUDRLRRDDR
-- 788
main :: IO ()
main =
 do input <- B8.pack <$> [format|2016 17 %s%n|]
    let paths = [path | (C 3 3, path) <- bfs (nextStates input) initialState ]
        shortestPath = head paths
        longestPath  = last paths
    putStrLn shortestPath
    print (length longestPath)

initialState :: (Coord, String)
initialState = (origin, "")

isValidLocation :: Coord -> Bool
isValidLocation (C y x) = 0 <= x && x < 4 && 0 <= y && y < 4

nextStates :: ByteString -> (Coord, String) -> [(Coord, String)]
nextStates _ (C 3 3,path) = []
nextStates input (c, path) =
  [ (c', path++[step])
  | (step, delta) <- directions input path
  , let c' = c + delta
  , isValidLocation c'
  ]

directions :: ByteString -> String -> [(Char, Coord)]
directions input path = ways
  where
    h = hash (input <> B8.pack path)

    isOpen x = 0xb <= x && x <= 0xf

    ways = [ ('U', north) | isOpen (BS.index h 0 `shiftR` 4) ] ++
           [ ('D', south) | isOpen (BS.index h 0 .&. 0xf)    ] ++
           [ ('L', west)  | isOpen (BS.index h 1 `shiftR` 4) ] ++
           [ ('R', east)  | isOpen (BS.index h 1 .&. 0xf)    ]
