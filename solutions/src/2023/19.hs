{-# Language DataKinds, DeriveTraversable, GADTs, ImportQualifiedPost, PatternSynonyms, QuasiQuotes, TemplateHaskell, ViewPatterns #-}
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

-- | A part is a quadruple of parameters indexed by 'V'
data Part a = Part a a a a
  deriving (Functor, Foldable, Traversable, Show)

-- | 'V' is an index into a field of a 'Part'
data V = Vx | Vm | Va | Vs

-- | Less-than and greater-than comparison operators for the workflow rules.
data O = O_LT | O_GT

-- | 'Ints' is a range of 'Int' with an inclusive lower bound and exclusive upper bound.
type Ints = Box' 1

-- | A rule is a part field, an operator, a bound, and a jump target
type Rule = (V, O, Int, String)

stageTH

-- | Parse the input instructions and print both parts.
--
-- >>> :main
-- 397134
-- 127517902575337
main :: IO ()
main =
 do (workflows_, parts_) <- [format|2023 19 (%a+{(@V@O%u:%a+,)*%a+}%n)*%n({x=%u,m=%u,a=%u,s=%u}%n)*|]
    let workflows = Map.fromList [(k, (rs, e)) | (k, rs, e) <- workflows_]
        parts = [Part x m a s | (x, m, a, s) <- parts_]
    print (sum [sum p | p <- parts, accepted workflows p])
    let full = 1 :> 4001
    print (acceptedCount workflows (Part full full full full))

-- | Predicate for parts that will be accepted by the workflow.
accepted :: Map String ([Rule], String) -> Part Int -> Bool
accepted workflows xmas = 0 /= acceptedCount workflows (fmap one xmas)
  where
    one i = i :> i + 1 -- single-element interval

-- | Count of the number of distinct parts that are accepted by the workflow.
acceptedCount :: Map String ([Rule], String) -> Part Ints -> Int
acceptedCount workflows = jump "in"
  where
    jump "A"                             = product . fmap size
    jump "R"                             = const 0
    jump ((workflows Map.!) -> (rs, el)) = foldr rule (jump el) rs

    rule (var, O_GT, n, tgt) continue p =
      case split (n + 1) (lkp p var) of
        (lo, hi) ->
          maybe 0 (continue . set p var) lo +
          maybe 0 (jump tgt . set p var) hi

    rule (var, O_LT, n, tgt) continue p =
      case split n (lkp p var) of
        (lo, hi) ->
          maybe 0 (jump tgt . set p var) lo +
          maybe 0 (continue . set p var) hi

-- | Divide an interval into a region below and at a split.
split :: Int -> Ints -> (Maybe Ints, Maybe Ints)
split n r@(lo :> hi)
  | n <= lo   = (Nothing       , Just r        )
  | n >= hi   = (Just r        , Nothing       )
  | otherwise = (Just (lo :> n), Just (n :> hi))

-- | Field accessor for 'Part'
lkp :: Part a -> V -> a
lkp (Part x _ _ _) Vx = x
lkp (Part _ m _ _) Vm = m
lkp (Part _ _ a _) Va = a
lkp (Part _ _ _ s) Vs = s

-- | Field updater for 'Part'
set :: Part a -> V -> a -> Part a
set (Part _ m a s) Vx x = Part x m a s
set (Part x _ a s) Vm m = Part x m a s
set (Part x m _ s) Va a = Part x m a s
set (Part x m a _) Vs s = Part x m a s

-- | Interval constructor: inclusive lower-bound, exclusive upper-bound.
-- Invariant: lower-bound < upper-bound
pattern (:>) :: Int -> Int -> Ints
pattern lo :> hi = Dim lo hi Pt
infix 4 :>
{-# COMPLETE (:>) #-}
