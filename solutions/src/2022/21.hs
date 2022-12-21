{-# Language PatternSynonyms, DeriveTraversable, QuasiQuotes, BlockArguments, LambdaCase, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/21>

>>> :{
:main +
  "root: pppw + sjmn\n\
  \dbpl: 5\n\
  \cczh: sllz + lgvd\n\
  \zczc: 2\n\
  \ptdq: humn - dvpt\n\
  \dvpt: 3\n\
  \lfqf: 4\n\
  \humn: 5\n\
  \ljgn: 2\n\
  \sjmn: drzm * dbpl\n\
  \sllz: 4\n\
  \pppw: cczh / lfqf\n\
  \lgvd: ljgn * ptdq\n\
  \drzm: hmdt - zczc\n\
  \hmdt: 32\n"
:}
152
301

-}
module Main where

import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map

import Advent (format)
import Advent.Fix (Fix(Fix))

-- |
-- >>> :main
-- 110181395003396
-- 3721298272959
main :: IO ()
main =
 do input <- buildMap <$> [format|2022 21 (%s: (%d|%s %c %s)%n)*|]
    
    -- part 1
    case evalRoot input of
      I p1 -> print p1
      _ -> fail "unsupported part 1"
    
    -- part 2
    case evalRoot (Map.insert "humn" Answer input) of
      Fix (Add x (I y)) -> print (equal x y)
      _ -> fail "unsupported part 2"

evalRoot :: Map String (Expr String) -> Fix Expr
evalRoot env = env' Map.! "root"
  where env' = fmap (constProp . tie env') env

tie :: (Functor f, Ord a) => Map a (Fix f) -> f a -> Fix f
tie m e = Fix (fmap (m Map.!) e)

buildMap :: [(String, Either Int (String, Char, String))] -> Map String (Expr String)
buildMap xs =
  Map.fromList xs <&> \case
    Left i          -> Lit i
    Right (a,'*',b) -> Mul a b
    Right (a,'+',b) -> Add a b
    Right (a,'/',b) -> Div a b
    Right (a,'-',b) -> Sub a b
    _               -> error "bad expression"

constProp :: Fix Expr -> Fix Expr
constProp = \case
  Fix (Add (I x) (I y)) -> I (x+y)
  Fix (Sub (I x) (I y)) -> I (x-y)
  Fix (Mul (I x) (I y)) -> I (x*y)
  Fix (Div (I x) (I y)) | (z,0) <- x `quotRem` y -> I z
  e -> e

equal :: Fix Expr -> Int -> Int
equal (Fix (Div x (I y))) z = equal x (y*z)
equal (Fix (Add (I x) y)) z = equal y (z-x)
equal (Fix (Add x (I y))) z = equal x (z-y)
equal (Fix (Mul (I x) y)) z | (z',0) <- z `quotRem` x = equal y z'
equal (Fix (Mul x (I y))) z | (z',0) <- z `quotRem` y = equal x z'
equal (Fix (Sub (I x) y)) z = equal y (x-z)
equal (Fix (Sub x (I y))) z = equal x (y+z)
equal (Fix Answer) x = x
equal _ _ = error "stuck"

pattern I :: Int -> Fix Expr
pattern I i = Fix (Lit i)

data Expr a
    = Add a a
    | Sub a a
    | Mul a a
    | Div a a
    | Lit Int
    | Answer
    deriving (Show, Functor)
