{-# Language QuasiQuotes, BlockArguments, LambdaCase, ImportQualifiedPost #-}
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
import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

data Pad = Pad (Set Coord) (Map Char Coord)

padFromList :: [(Coord, Char)] -> Pad
padFromList xs = Pad (Set.fromList [p | (p, _) <- xs]) (Map.fromList [(c,p) | (p,c) <- xs])

padCoord :: Pad -> Char -> Coord
padCoord (Pad _ m) c = m Map.! c

inPad :: Pad -> Coord -> Bool
inPad (Pad s _) x = Set.member x s

robotPad :: Pad
robotPad = padFromList [(C 0 (-1), '^'), (C 0 0, 'A'), (C 1 (-2), '<'), (C 1 (-1), 'v'), (C 1 0, '>')]

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
 do let deltas = padDeltas doorPad str
    keys <- concat <$> traverse deltaToKeys deltas
    guard (validate doorPad keys)
    pure keys


robotCostN :: Int -> String -> Int
robotCostN = memo2 \n str ->
  if n == 0 then length str
    else
      minimum $
     do let deltas = padDeltas robotPad str
        keys <- concat <$> traverse deltaToKeys deltas
        guard (validate robotPad keys)
        pure (sum (map (robotCostN (n-1)) (splitA keys)))

splitA :: [Char] -> [[Char]]
splitA [] = []
splitA xs =
  case break ('A'==) xs of
    (l,a:r) -> (l ++ [a]) : splitA r
    _ -> undefined

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

-- | >>> :main
-- 177814
-- 220493992841852
main :: IO ()
main =
 do codes <- getInputLines 2024 21
    let score n x = (read (takeWhile isDigit x) * answer n x)
    print (sum (map (score 2) codes))
    print (sum (map (score 25) codes))

answer :: Int -> String -> Int
answer n str = minimum (map (robotCostN n) (initialStrings str))

{-
doorPad :: Map Coord Char
doorPad = Map.fromList
  [(C (-3) (-2), '7')
  ,(C (-3) (-1), '8')
  ,(C (-3) (0) , '9')
  ,(C (-2) (-2), '4')
  ,(C (-2) (-1), '5')
  ,(C (-2) (0) , '6')
  ,(C (-1) (-2), '1')
  ,(C (-1) (-1), '2')
  ,(C (-1) (0) , '3')
  , (C 0 (-1)  , '0')
  , (C 0 0     , 'A')
  ]

doorMap :: Map Char Coord
doorMap = Map.fromList [(v,k) | (k,v) <- Map.assocs doorPad  ]
robotMap :: Map Char Coord
robotMap = Map.fromList [(v,k) | (k,v) <- Map.assocs robotPad  ]


robotPad :: Map Coord Char
robotPad = Map.fromList
  [(C 0 (-1), '^'), (C 0 0, 'A'), (C 1 (-2), '<'), (C 1 (-1), 'v'), (C 1 0, '>')]

doorShort :: String -> [(Char, Int)]
doorShort text = go origin coords
  where
    coords = map (doorMap Map.!) text

    go here [] = []
    go (C y1 x1) (C y2 x2:xs) =
      (if y2 < y1 then (('^', y1-y2):) else id) $
      (if x1 < x2 then (('>', x2-x1):) else id) $
      (if y1 < y2 then (('v', y2-y1):) else id) $
      (if x2 < x1 then (('<', x1-x2):) else id) $
      ('A',1) :
      go (C y2 x2) xs

robotShort :: String -> [(Char, Int)]
robotShort text = go origin coords
  where
    coords = map (robotMap Map.!) text

    go here [] = []
    go (C y1 x1) (C y2 x2:xs) =
      (if y2 < y1 then (('^', y1-y2):) else id) $
      (if x1 < x2 then (('>', x2-x1):) else id) $
      (if x2 < x1 then (('<', x1-x2):) else id) $
      (if y1 < y2 then (('v', y2-y1):) else id) $
      ('A',1) :
      go (C y2 x2) xs

sequencesD :: String -> [String]
sequencesD = go (C 0 0)
  where
    go here = \case
      [] -> [""]
      x:xs ->
        do let target = stuff Map.! x
           p <- pd here target
           q <- go target xs
           pure (p ++ q)

    stuff = Map.fromList [(v,k) | (k,v) <- Map.assocs doorPad  ]

sequencesR :: String -> [String]
sequencesR = go (C 0 0)
  where
    go here = \case
      [] -> [""]
      x:xs ->
        do let target = stuff Map.! x
           p <- paths robotPad here target
           q <- go target xs
           pure (p ++ q)

    stuff = Map.fromList [(v,k) | (k,v) <- Map.assocs robotPad  ]

paths :: Map Coord a -> Coord -> Coord -> [[Char]]
paths layout here@(C y1 x1) tgt@(C y2 x2)
  | Map.notMember here layout = []
  | here == tgt = ["A"]
  | otherwise =
      (if y2 < y1 then map (replicate (y1-y2) '^' ++ ) (paths layout (C y2 x1) tgt) else []) ++
      (if x1 < x2 then map (replicate (x2-x1) '>' ++ ) (paths layout (C y1 x2) tgt) else []) ++
      (if y1 < y2 && Map.member (C y2 x1) layout then map (replicate (y2-y1) 'v' ++) (paths layout (C y2 x1) tgt) else []) ++
      (if x2 < x1 && Map.member (C y1 x2) layout then map (replicate (x1-x2) '<' ++) (paths layout (C y1 x2) tgt) else [])

pd :: Coord -> Coord -> [[Char]]
pd = memo2 (paths doorPad)


main :: IO ()
main =
  -- withArgs ["/Users/emertens/Source/advent/example.txt"]
    do
    input <- [format|2024 21 (%s%n)*|]
    -- input <- getInputArray 2024 21
    print $ sum $ map score input

shortest xs = filter (\x -> length x == n) xs
   where
    n = minimum (map length xs)

score x = (read (takeWhile isDigit x) * answer x :: Integer)

answer :: String -> Integer
answer x = minimum $ map (\m -> sum [toInteger $ length k * v | (k,v)<- Map.assocs m]) $
  do
   a <- sequencesD x
   let a' = counts (splitA a)
   let go 0 y = y
       go n y = go (n-1) (Map.fromListWith (+) [(k', v) | (k,v ) <- Map.assocs y, k' <- splitA (pr k)])
   pure (go 2 a')

shortRobotFast xs = concatMap pr (splitA xs)

splitA [] = []
splitA xs =
  case break ('A'==) xs of
    (l,a:r) -> (l ++ [a]) : splitA r

pr :: String -> [Char]
pr = memo (\a ->
  let phase1 =  (sequencesR a)
  in minimumBy (comparing (minimum . map length . sequencesR)) phase1)
-}

-- 435231441040286 high
-- 250698925939238 high
-- 220493992841852 ???