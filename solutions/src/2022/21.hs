{-# Language QuasiQuotes, BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2022/day/21>

-}
module Main where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.SBV
    ( sat,
      sbvExists,
      SInteger,
      constrain,
      SymVal(literal),
      EqSymbolic((.==)),
      SDivisible(sMod, sDiv),
      Modelable(getModelValue),
      SatResult )
import Data.Foldable (traverse_)
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
    result <- part2 input
    traverse_ print (getModelValue "humn" result :: Maybe Integer)

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

part2 :: Input -> IO SatResult
part2 xs = sat
 do humn <- sbvExists "humn"
    let m :: Map String (SInteger)
        m = Map.fromList [(k, if k == "humn" then humn else eval e) | (k,e) <- xs]
        
        eval (Left i) = literal (fromIntegral i) :: SInteger
        eval (Right (a,'*',b)) = (m Map.! a) * (m Map.! b)
        eval (Right (a,'+',b)) = (m Map.! a) + (m Map.! b)
        eval (Right (a,'/',b)) = (m Map.! a) `sDiv` (m Map.! b)
        eval (Right (a,'-',b)) = (m Map.! a) - (m Map.! b)
        eval _ = error "bad expression"

    sequence_ [constrain (sMod (m Map.! a) (m Map.! b) .== 0) | (_, Right(a,'/',b)) <- xs]
    
    case lookup "root" xs of
        Just (Right (r1,_,r2)) -> pure (m Map.! r1 .== m Map.! r2)
        _ -> fail "missing root"
    