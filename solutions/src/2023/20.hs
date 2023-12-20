{-# Language QuasiQuotes, TemplateHaskell, BangPatterns, LambdaCase, ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Main
Description : Day 20 solution
Copyright   : (c) Eric Mertens, 2023
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2023/day/20>

This problem requires you to hack around inside your input file,
so if this solution doesn't work on yours, you didn't get lucky
and get a easier case like I did, but I assume we all got the same
kind of dumb easy case as the last LCM problem this year.

-}
module Main (main) where

import Advent (format, stageTH)
import Advent.Queue as Queue
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Kind sigil
data K = K | K_PERCENT | K_AMPERSAND
  deriving (Eq, Ord, Show)

data Kind
  = Broadcast                -- ^ broadcast node
  | Flipflop                 -- ^ flip-flop
  | Conjunction (Set String) -- ^ conjunction gate

data Stream a = a :| Stream a

stageTH

-- | Parse the input and print both parts.
--
-- >>> :main
-- 825167435
-- 225514321828633
main :: IO ()
main =
 do input <- [format|2023 20 (@K%a+ -> %a+&(, )%n)*|]
    let incoming = Map.fromListWith (++) [(k, [v]) | (_, v, ks) <- input, k <- ks]
    let nodes = Map.fromList [(name, (mkKind incoming name kind, conns)) | (kind, name, conns) <- input]

    print (part1 0 0 0 (sim nodes))
    print (part2 incoming (sim nodes))

mkKind :: Map String [String] -> String -> K -> Kind
mkKind incoming name = \case
  K           -> Broadcast
  K_AMPERSAND -> Conjunction (Set.fromList [name ++ " " ++ x | x <- incoming Map.! name])
  K_PERCENT   -> Flipflop

part1 :: Int -> Int -> Int -> Stream (String, a, Bool) -> Int
part1 n l h ((src,_,sig) :| xs)
  | n == 1000, src == "button" = l * h
  | otherwise =
    part1 (if src == "button" then n+1 else n)
          (if sig then l else l+1)
          (if sig then h+1 else h) xs

part2 :: Map String [String] -> Stream (String, String, Bool) -> Int
part2 incoming msgs = foldl1 lcm [buttonsFor 0 gate msgs | gate <- incoming Map.! specialConj]
  where
    [specialConj] = incoming Map.! "rx"

    buttonsFor n gate ((src, dst, msg) :| xs)
      | "button" == src = buttonsFor (n+1) gate xs
      | msg, src == gate, dst == specialConj = n
    buttonsFor n gate (_ :| xs) = buttonsFor n gate xs

sim :: Map String (Kind, [String]) -> Stream (String, String, Bool)
sim fwd = go Set.empty Queue.Empty
  where
    go st Queue.Empty = go st (Queue.singleton ("button", "broadcaster", False))
    go st ((src, dst, msg) Queue.:<| q') =
      (src, dst, msg) :|
      case Map.lookup dst fwd of
        Just (Broadcast, next) -> continue st msg next -- forward message
        Just (Flipflop, next)
          | not msg -> continue st' out next -- was on sends low
          where
            st' = mark dst out st
            out = not (Set.member dst st)
        Just (Conjunction incoming, next) -> continue st' out next
          where
            st' = mark (dst ++ " " ++ src) msg st
            out = not (incoming `Set.isSubsetOf` st')
        _ -> go st q' -- ignored
      where
        continue st' msg' next = go st' (Queue.appendList q' [(dst, t, msg') | t <- next])

mark :: Ord a => a -> Bool -> Set a -> Set a
mark key True = Set.insert key
mark key False = Set.delete key
