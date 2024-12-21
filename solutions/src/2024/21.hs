{-# Language BlockArguments, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2024
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2024/day/21>

>>> :{
:main + "029A
980A
179A
456A
379A
"
:}
126384
154115708116294

-}
module Main (main) where

import Advent (getInputLines)
import Advent.Coord (above, below, left, origin, right, Coord(..))
import Advent.Memo (memo2)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | >>> :main
-- 177814
-- 220493992841852
main :: IO ()
main =
 do codes <- getInputLines 2024 21
    let score n x = (read (init x) * answer n x)
    print (sum (map (score 2) codes))
    print (sum (map (score 25) codes))

answer :: Int -> String -> Int
answer n str = minimum (map (robotCostN n) (initialStrings str))

data Pad = Pad (Set Coord) (Map Char Coord)

padFromList :: [(Coord, Char)] -> Pad
padFromList xs = Pad (Set.fromList [p | (p, _) <- xs]) (Map.fromList [(c,p) | (p,c) <- xs])

padCoord :: Pad -> Char -> Coord
padCoord (Pad _ m) c = m Map.! c

inPad :: Pad -> Coord -> Bool
inPad (Pad s _) x = Set.member x s

-- | The 4-direction pad centered on the @A@ button.
robotPad :: Pad
robotPad = padFromList [(C 0 (-1), '^'), (C 0 0, 'A'), (C 1 (-2), '<'), (C 1 (-1), 'v'), (C 1 0, '>')]

-- | The 10-digit pad centered on the @A@ button.
doorPad :: Pad
doorPad = padFromList
  [ (C (-3) (-2), '7')
  , (C (-3) (-1), '8')
  , (C (-3) 0   , '9')
  , (C (-2) (-2), '4')
  , (C (-2) (-1), '5')
  , (C (-2) 0   , '6')
  , (C (-1) (-2), '1')
  , (C (-1) (-1), '2')
  , (C (-1) (0) , '3')
  , (C 0 (-1)   , '0')
  , (C 0 0      , 'A')
  ]

initialStrings :: String -> [String]
initialStrings str =
   [ keys
   | let deltas = padDeltas doorPad str
   , keys <- concat <$> traverse deltaToKeys deltas
   , validate doorPad keys
   ]

robotCostN :: Int -> String -> Int
robotCostN = memo2 \n str ->
  if n == 0 then length str else
  minimum
    [ sum (map (robotCostN (n-1)) keys)
    | let deltas = padDeltas robotPad str
    , keys <- traverse deltaToKeys deltas
    , validate robotPad (concat keys)
    ]

validate :: Pad -> [Char] -> Bool
validate pad str = all (inPad pad) posns
  where
    posns = scanl move origin str
    move here 'A' = here
    move here '>' = right here
    move here '<' = left here
    move here '^' = above here
    move here 'v' = below here
    move _ _ = undefined

padDeltas :: Pad -> String -> [Coord]
padDeltas pad str = zipWith (-) (absolutes) (origin:absolutes)
  where
    absolutes = map (padCoord pad) str

deltaToKeys :: Coord -> [String]
deltaToKeys (C y x) =
  [ keys ++ "A"
  | let rawKeys =
          (if y < 0 then (replicate (-y) '^' ++) else id) $
          (if x > 0 then (replicate x '>' ++) else id) $
          (if y > 0 then (replicate y 'v' ++) else id) $
          (if x < 0 then replicate (-x) '<' else "")
  , keys <- rawKeys : [reverse rawKeys | y /= 0, x /= 0]
  ]
