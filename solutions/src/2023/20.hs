{-# Language QuasiQuotes, TemplateHaskell, BangPatterns, BlockArguments, LambdaCase, ImportQualifiedPost #-}
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
module Main where

import Advent (format, stageTH)
import Advent.Queue as Queue
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Kind of node in the graph
data K
  = K           -- ^ broadcast node
  | K_PERCENT   -- ^ flip-flop
  | K_AMPERSAND -- ^ conjunction gate
  deriving (Eq, Ord, Show)

stageTH

-- | Parse the input and print both parts.
--
-- >>> :main
-- 825167435
-- 225514321828633
main :: IO ()
main =
 do input <- [format|2023 20 (@K%a+ -> %a+&(, )%n)*|]
    let nodes = Map.fromList [(name, (kind, conns)) | (kind, name, conns) <- input]
    let conj = Map.fromListWith (++) [(v, [k]) | (k, (_, vs)) <- Map.assocs nodes, v <- vs]    
    
    let part1 !n !l !h (("button",_,_):_) | n == (1000 :: Int) = l * h :: Int
        part1 n l h ((src,_,sig):xs) =
            part1 (if src == "button" then n+1 else n)
                  (if sig then l else l+1)
                  (if sig then h+1 else h) xs
        part1 _ _ _ [] = error "part1 failed"

    print (part1 0 0 0 (sim conj nodes))
    
    -- This is one of those sad days that you have to look at your own input :(
    print (part2 conj (sim conj nodes))


part2 :: Map String [String] -> [(String, String, Bool)] -> Int
part2 incoming msgs = foldl1 lcm [buttonsFor 0 dude msgs | dude <- incoming Map.! specialConj]
  where
    [specialConj] = incoming Map.! "rx"

    buttonsFor n gate (("button", _, _):xs) = buttonsFor (n+1) gate xs
    buttonsFor n gate ((src, dst, True):_) | src == gate, dst == specialConj = n
    buttonsFor n gate (_:xs) = buttonsFor n gate xs
    buttonsFor _ _ _ = undefined

sim :: Map String [String] -> Map String (K, [String]) -> [(String, String, Bool)]
sim conj conns = go Set.empty Queue.Empty
  where
    go st q =
      case Queue.pop q of
        Nothing -> go st (Queue.singleton ("button", "broadcaster", False))
        Just ((src, dst, msg), q') ->
          (src, dst, msg) :
          case Map.lookup dst conns of
            Nothing -> go st q' -- output node, keep going
            Just (K, next) ->
              go st (appendList q' [(dst, t, msg) | t <- next])
            Just (K_PERCENT, next)
              | msg -> go st q' -- ignored
              | otherwise -> -- was on sends low
                  let active = not (Set.member dst st)
                      outmsgs = [(dst, t, active) | t <- next]
                  in go (mark dst active st) (appendList q' outmsgs)
            Just (K_AMPERSAND, next) ->
              let st1 = mark (src ++ " " ++ dst) msg st
                  out = not (and [Set.member (inp ++ " " ++ dst) st1 | inp <- conj Map.! dst])
                  outmsgs = [(dst, t, out) | t <- next]
              in go st1 (Queue.appendList q' outmsgs)

mark :: Ord a => a -> Bool -> Set a -> Set a
mark key True = Set.insert key
mark key False = Set.delete key
