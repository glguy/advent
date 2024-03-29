{-# Language QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 25 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/25>

>>> :{
:main +
    "1=-0-2\n\
    \12111\n\
    \2=0=\n\
    \21\n\
    \2=01\n\
    \111\n\
    \20012\n\
    \112\n\
    \1=-1=\n\
    \1-12\n\
    \12\n\
    \1=\n\
    \122\n"
:}
2=-1=0

-}
module Main where

import Advent (format)

-- |
-- >>> :main
-- 20-==01-2-=1-2---1-0
main :: IO ()
main =
 do input <- [format|2022 25 (%s%n)*|]
    putStrLn (toSnafu (sum (map fromSnafu input)))

fromSnafu :: String -> Int
fromSnafu = foldl f 0
  where
    f acc c = 5 * acc + fromS c

toSnafu :: Int -> String
toSnafu = go ""
  where
    go acc 0 = acc
    go acc n = go (toS (m-2) : acc) n'
      where
        (n',m) = (n+2) `divMod` 5

toS :: Int -> Char
toS (-2) = '='
toS (-1) = '-'
toS 0 = '0'
toS 1 = '1'
toS 2 = '2'
toS _ = error "toS: bad digit"

fromS :: Char -> Int
fromS '2' = 2
fromS '1' = 1
fromS '0' = 0
fromS '-' = -1
fromS '=' = -2
fromS _ = error "fromS: bad digit"
