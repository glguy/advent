{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 15 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/15>

This problem has us follow a lens update sequence. The solution
below stores the lenses in an array and uses accumArray to
efficiently apply updates to that array in-place.

>>> :main + "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"
1320
145

-}
module Main (main) where

import Advent (format)
import Data.Array (accumArray, assocs)
import Data.Char (ord)

-- | Parse the input sequence and print the results of both parts.
--
-- >>> :main
-- 503487
-- 261505
main :: IO ()
main =
 do input <- [format|2023 15 (%a+(-|=%d))!&,%n|]
    print (sum [hash raw | (raw, _) <- input])

    let boxes = accumArray updateBox [] (0, 255)
                  [(hash lbl, (lbl, cmd)) | (_, (lbl, cmd)) <- input]

    print (sum [ (1+box) * i * len
               | (box, xs)     <- assocs boxes
               , (i, (_, len)) <- zip [1..] xs])

-- | Run the HASH algorithm on an input string.
hash :: String -> Int
hash str = foldl (\acc x -> 17 * (ord x + acc)) 0 str `rem` 256

-- | Either update the focal length or remove a lens by label of a lens box.
updateBox ::
  [(String, Int)]     {- ^ lens box                -} ->
  (String, Maybe Int) {- ^ label, new focal length -} ->
  [(String, Int)]     {- ^ updated lens box        -}
updateBox prev (lbl, Nothing) = filter ((lbl /=) . fst) prev
updateBox prev (lbl, Just n ) = go prev
  where
    go ((k,_) : xs) | lbl == k = (lbl, n) : xs
    go (x     : xs)            = x : go xs
    go []                      = [(lbl, n)]
