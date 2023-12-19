{-# Language QuasiQuotes, TemplateHaskell, GADTs, DataKinds, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/19>

>>> :{
:main +
"px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}\n
{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
"
:}
19114
167409079868000

-}
module Main (main) where

import Advent (format, stageTH)
import Advent.Box (size, Box(Pt, Dim), Box')
import Data.Map (Map)
import Data.Map qualified as Map

data V = Vx | Vm | Va | Vs

stageTH

-- | Parse the input instructions and print both parts.
--
-- >>> :main
-- 397134
-- 127517902575337
main :: IO ()
main =
 do (workflows, parts) <- [format|2023 19 (%a+{(@V>%d:%a+|@V<%d:%a+|%a+)&,}%n)*%n({x=%d,m=%d,a=%d,s=%d}%n)*|]
    let workflowMap = Map.fromList [(k, map toRule v) | (k, v) <- workflows]
    print (sum [rating1 workflowMap "in" p | p <- parts])
    let full = Dim 1 4001 Pt
    print (rating2 workflowMap "in" (full,full,full,full))

data Rule = LessThan V Int String | GreaterThan V Int String | Else String

toRule :: Either (Either (V, Int, String) (V, Int, String)) String -> Rule
toRule (Left (Left  (v, n, lbl))) = GreaterThan v n lbl
toRule (Left (Right (v, n, lbl))) = LessThan    v n lbl
toRule (Right              lbl  ) = Else            lbl

rating1 :: Map String [Rule] -> String -> (Int, Int, Int, Int) -> Int
rating1 _ "A" (x, m, a, s) = x + m + a + s
rating1 _ "R" _ = 0
rating1 workflows k p = process (workflows Map.! k)
  where
    process (GreaterThan var n tgt : rest)
       | lkp p var > n = rating1 workflows tgt p
       | otherwise     = process rest
    process (LessThan var n tgt : rest)
       | lkp p var < n = rating1 workflows tgt p
       | otherwise     = process rest
    process (Else tgt : _) = rating1 workflows tgt p
    process [] = error "bad rule"

rating2 :: Map String [Rule] -> String -> (Box' 1, Box' 1, Box' 1, Box' 1) -> Int
rating2 _ "A" (x,m,a,s) = size x * size m * size a * size s
rating2 _ "R" _ = 0
rating2 workflows k p0 =
  process (workflows Map.! k) p0
  where
    process (GreaterThan var n tgt : rest) p =
      case lkp p var of
        Dim lo hi Pt ->
          (if n+1 < hi then rating2 workflows tgt (set p var (Dim (n+1) hi Pt)) else 0) +
          (if lo < n+1 then process rest (set p var (Dim lo (n+1) Pt)) else 0)

    process (LessThan var n tgt : rest) p =
      case lkp p var of
        Dim lo hi Pt ->
          (if lo < n then rating2 workflows tgt (set p var (Dim lo n Pt) ) else 0) +
          (if n < hi then process rest (set p var (Dim n hi Pt)) else 0)

    process (Else tgt : _) p = rating2 workflows tgt p

    process [] _ = error "bad rule"

lkp :: (a, a, a, a) -> V -> a
lkp (x,_,_,_) Vx = x
lkp (_,m,_,_) Vm = m
lkp (_,_,a,_) Va = a
lkp (_,_,_,s) Vs = s

set :: (d, d, d, d) -> V -> d -> (d, d, d, d)
set (_,m,a,s) Vx x = (x,m,a,s)
set (x,_,a,s) Vm m = (x,m,a,s)
set (x,m,_,s) Va a = (x,m,a,s)
set (x,m,a,_) Vs s = (x,m,a,s)
