{-# Language ImportQualifiedPost, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 8 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2021/day/8>

Figure out how the miswired segment display works.

>>> :{
:main +
"acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
"
:}
0
5353

>>> :{
:main +
"be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
"
:}
26
61229

-}
module Main (main) where

import Advent (countBy, format, fromDigits)
import Data.Bits (setBit)
import Data.Char (ord)
import Data.List (permutations, foldl')
import Data.Map (Map)
import Data.Map qualified as Map

-- | >>> :main
-- 355
-- 983030
main :: IO ()
main =
 do inp <- [format|2021 8 ((%s )*%|( %s)*%n)*|]
    let outs = map solve inp
    print (countBy (`elem` [1,4,7,8]) (concat outs))
    print (sum (map (fromDigits 10) outs))

wires :: String
wires = ['a'..'g']

-- | Mapping from active line segments to digits using this arrangement.
--
-- @
--   0:      1:      2:      3:      4:
--  aaaa    ....    aaaa    aaaa    ....
-- b    c  .    c  .    c  .    c  b    c
-- b    c  .    c  .    c  .    c  b    c
--  ....    ....    dddd    dddd    dddd
-- e    f  .    f  e    .  .    f  .    f
-- e    f  .    f  e    .  .    f  .    f
--  gggg    ....    gggg    gggg    ....
--
--   5:      6:      7:      8:      9:
--  aaaa    aaaa    aaaa    aaaa    aaaa
-- b    .  b    .  .    c  b    c  b    c
-- b    .  b    .  .    c  b    c  b    c
--  dddd    dddd    ....    dddd    dddd
-- .    f  e    f  .    f  e    f  .    f
-- .    f  e    f  .    f  e    f  .    f
--  gggg    gggg    ....    gggg    gggg
-- @
digits :: Map String Int
digits = Map.fromList (zip ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"] [0..9])

-- | All the possible reassignments of wires
mappings :: [Map Int Int]
mappings =
  [ Map.mapKeys (toBitMask . map (assignment Map.!)) digits
    | wires' <- permutations wires
    , let assignment = Map.fromList (zip wires wires')
  ]

-- | Given a list of segment examples and outputs decode the outputs.
solve :: ([String], [String]) -> [Int]
solve (xs, ys) = head
  [ out
  | mapping <- mappings
  , let rewire x = Map.lookup (toBitMask x) mapping
  , Just out <- [traverse rewire xs *> traverse rewire ys]
  ]

-- | Convert the segment labels to a more efficient characteristic 'Int'
toBitMask :: String -> Int
toBitMask = foldl' (\acc x -> setBit acc (ord x - ord 'a')) 0
