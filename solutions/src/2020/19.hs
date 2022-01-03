{-# Language ImportQualifiedPost, OverloadedStrings, QuasiQuotes #-}
{-|
Module      : Main
Description : Day 19 solution
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2020/day/19>

Run a parser define by a simple list of alternatives of sequences or
literal matches.

-}
module Main (main) where

import Advent (countBy)
import Advent.Format (format)
import Data.Foldable (traverse_, asum)
import Data.Functor (void)
import Data.IntMap.Strict (IntMap)
import Data.IntMap qualified as IntMap
import Advent.ReadS (P(..), char, eof)

-- | Rules either match a literal string, or match a sum
-- of product of sub-rules.
type Rule = Either Char [[Int]]

------------------------------------------------------------------------

-- | Print answers.
--
-- >>> :main
-- 180
-- 323
main :: IO ()
main =
  do (rs,ws) <- [format|19 (%u: ("%c"|%u& &( %| ))%n)*%n(%s%n)*|]
     let rules1 = IntMap.fromList rs
         rules2 = IntMap.insert  8 (Right [[42   ],[42, 8   ]])
                $ IntMap.insert 11 (Right [[42,31],[42,11,31]])
                $ rules1
     print (run rules1 ws)
     print (run rules2 ws)

-- | Count the number of input strings that satisfy the parse rules.
run ::
  IntMap Rule {- ^ parse rules                      -} ->
  [String]    {- ^ input strings                    -} ->
  Int         {- ^ number of matching input strings -}
run rules = countBy (not . null . topParser)
  where
    topParser :: ReadS ()
    P topParser = parsers IntMap.! 0 >> eof

    parsers :: IntMap (P ())
    parsers = ruleParser <$> rules

    ruleParser :: Rule -> P ()
    ruleParser = either (void . char) (asum . map order)

    order :: [Int] -> P ()
    order = traverse_ (parsers IntMap.!)
