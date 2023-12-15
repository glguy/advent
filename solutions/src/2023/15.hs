{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/15>

>>> :main + "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"
1320
145

-}
module Main where

import Advent (format)
import Data.Array (accumArray, assocs)
import Data.Char (ord)

-- |
--
-- >>> :main
-- 503487
-- 261505
main :: IO ()
main =
 do input <- [format|2023 15 (%a+(-|=%d))!&,%n|]
    print (sum [hasher raw | (raw, _) <- input])

    let boxes = accumArray apply [] (0, 255)
                  [(hasher lbl, (lbl, cmd)) | (_, (lbl, cmd)) <- input]

    print (sum [ (1+box) * i * len
               | (box, xs)     <- assocs boxes
               , (i, (_, len)) <- zip [1..] xs])

hasher :: String -> Int
hasher str = foldl (\acc x -> 17 * (ord x + acc)) 0 str `rem` 256

apply :: [(String, Int)] -> (String, Maybe Int) -> [(String, Int)]
apply prev (lbl, Nothing) = filter ((lbl /=) . fst) prev
apply prev (lbl, Just n ) = go prev
  where
    go ((k,_) : xs) | lbl == k = (lbl, n) : xs
    go (x     : xs)            = x : go xs
    go []                      = [(lbl, n)]
