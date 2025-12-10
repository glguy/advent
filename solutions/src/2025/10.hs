{-# Language QuasiQuotes, BlockArguments #-}
{-|
Module      : Main
Description : Day 10 solution
Copyright   : (c) Eric Mertens, 2025
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2025/day/10>

>>> :{
:main +
"[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
"
:}
7
33

-}
module Main (main) where

import Advent (format)
import Data.Foldable (for_)
import Data.Maybe (fromJust)
import Data.SBV (SInteger, optLexicographic, free, minimize, (.==), (.>=), constrain, getModelValue)
import Data.Traversable (for)

-- | >>> :main
-- 409
-- 15489
main :: IO ()
main =
 do input <- [format|2025 10 ([%c*]( %(%d&,%))* {%d&,}%n)*|]
    print (sum (map part1 input))
    xs <- traverse part2 input
    print (sum xs)

part1 :: ([Char], [[Int]], [Int]) -> Int
part1 (goal, btns, _) = fromJust (cost assertion)
  where
    assertion =
      foldl conj R1
        [ foldl xorRME
            (if c == '#' then R0 else R1)
            [ Node b R0 R1
            | (b,btn) <- zip [0..] btns
            , i `elem` btn
            ]
        | (i, c) <- zip [0..] goal
        ]

-- | Fewest number of true assignments to satisfy boolean assertion
cost :: RME -> Maybe Int
cost R0 = Nothing
cost R1 = Just 0
cost (Node _ a b) =
  case cost a of
    Nothing -> (1+) <$> cost b
    Just a' -> Just $!
      case cost (xorRME a b) of
        Nothing -> a'
        Just b' -> min a' (1 + b')

part2 :: ([Char], [[Int]], [Int]) -> IO Integer
part2 (_, btns, jolt) =
  do
    res <- optLexicographic
      do -- allocate one, non-zero coefficient per button to press
        cs <- for [0 .. length btns - 1] \i ->
          do
            c <- free ("x" ++ show i)
            constrain (c .>= 0)
            pure (c :: SInteger)

        -- add a constraint for each element of the joltage
        for_ (zip [0 .. ] jolt) \(i, j) ->
          let j' = sum [c | (c, btn) <- zip cs btns, i `elem` btn]
          in constrain (j' .== fromIntegral j)

        -- optimize the problem for the smallest number of button presses
        minimize "smallest sum" (sum cs)

    case getModelValue "smallest sum" res of
      Just x -> pure x
      Nothing -> fail "no solution"

-- Local implementation of https://hackage-content.haskell.org/package/rme

-- | Boolean formulas in Algebraic Normal Form, using a representation
-- based on the Reed-Muller expansion.
data RME = Node !Int !RME !RME | R0 | R1
  deriving (Eq, Ord, Show)
-- Invariants: The last argument to a `Node` constructor should never
-- be `R0`. Also the `Int` arguments should strictly increase as you
-- go deeper in the tree.

-- | Normalizing constructor.
node :: Int -> RME -> RME -> RME
node _ a R0 = a
node n a b = Node n a b

-- | Logical complement.
compl :: RME -> RME
compl R0 = R1
compl R1 = R0
compl (Node n a b) = Node n (compl a) b

-- | Xor
xorRME :: RME -> RME -> RME
xorRME R0 y = y
xorRME R1 y = compl y
xorRME x R0 = x
xorRME x R1 = compl x
xorRME x@(Node i a b) y@(Node j c d)
  | i < j = Node i (xorRME a y) b
  | j < i = Node j (xorRME x c) d
  | otherwise = node i (xorRME a c) (xorRME b d)

-- | Conjunction
conj :: RME -> RME -> RME
conj R0 _ = R0
conj R1 y = y
conj _ R0 = R0
conj x R1 = x
conj x@(Node i a b) y@(Node j c d)
  | i < j = node i (conj a y) (conj b y)
  | j < i = node j (conj x c) (conj x d)
  | otherwise = node i ac (xorRME ac (conj (xorRME a b) (xorRME c d)))
  where ac = conj a c
