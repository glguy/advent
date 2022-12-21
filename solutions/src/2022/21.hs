{-# Language DeriveDataTypeable, QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/21>

-}
module Main where

import Data.Generics (Data, mkT, everywhere)
import Data.Map qualified as Map

import Advent (format)

type Input = [(String, Either Int (String, Char, String))]

-- |
-- >>> :main
-- 110181395003396
-- 3721298272959
main :: IO ()
main =
 do input <- [format|2022 21 (%s: (%d|%s %c %s)%n)*|]
    print (part1 input)
    let (l,r) = part2expr input
    print
      case (rewrite l, rewrite r) of
        (x, Lit y) -> equal x y
        (Lit x, y) -> equal y x
        _ -> error "not supported"
    

part1 :: Input -> Int
part1 xs = m Map.! "root"
  where
     m = Map.fromList 
            [(k, eval e)  | (k,e) <- xs]
     eval (Left i) = i
     eval (Right (a,'*',b)) = (m Map.! a) * (m Map.! b)
     eval (Right (a,'+',b)) = (m Map.! a) + (m Map.! b)
     eval (Right (a,'/',b)) = (m Map.! a) `div` (m Map.! b)
     eval (Right (a,'-',b)) = (m Map.! a) - (m Map.! b)
     eval _ = error "bad expression"

part2expr :: Input -> (Expr, Expr)
part2expr xs =
    case lookup "root" xs of
        Just (Right (r1,_,r2)) -> (m Map.! r1, m Map.! r2)
        _ -> error "missing root"
  where
    m = Map.fromList [(k, if k == "humn" then Answer else eval e) | (k,e) <- xs] 
    eval (Left i) = Lit i
    eval (Right (a,'*',b)) = Mul (m Map.! a) (m Map.! b)
    eval (Right (a,'+',b)) = Add (m Map.! a) (m Map.! b)
    eval (Right (a,'/',b)) = Div (m Map.! a) (m Map.! b)
    eval (Right (a,'-',b)) = Sub (m Map.! a) (m Map.! b)
    eval _ = error "bad expression"


constProp :: Expr -> Expr
constProp (Add (Lit x) (Lit y)) = Lit (x+y)
constProp (Sub (Lit x) (Lit y)) = Lit (x-y)
constProp (Mul (Lit x) (Lit y)) = Lit (x*y)
constProp (Div (Lit x) (Lit y)) | (z,0) <- x `quotRem` y = Lit z
constProp e = e

rewrite :: Expr -> Expr
rewrite = everywhere (mkT constProp)

equal :: Expr -> Int -> Int
equal (Div x (Lit y)) z = equal x (y*z)
equal (Add (Lit x) y) z = equal y (z-x)
equal (Add x (Lit y)) z = equal x (z-y)
equal (Mul (Lit x) y) z | (z',0) <- z `quotRem` x = equal y z'
equal (Mul x (Lit y)) z | (z',0) <- z `quotRem` y = equal x z'
equal (Sub (Lit x) y) z = equal y (x-z)
equal (Sub x (Lit y)) z = equal x (y+z)
equal Answer x = x
equal x y = error ("stuck: " ++ show (x,y))

data Expr
    = Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Lit Int
    | Answer
    | Equal Expr Expr
    deriving (Data, Show)
