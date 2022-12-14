{-# Language ImportQualifiedPost, BlockArguments #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-|
Module      : Advent.Prelude
Description : Prelude extension for AoC solutions
Copyright   : (c) Eric Mertens, 2018-2021
License     : ISC
Maintainer  : emertens@gmail.com

Various helper functions for common operations needed in
Advent of Code problems.

-}
module Advent.Prelude where

import Control.Applicative (Alternative(empty))
import Control.Monad.Trans.State (StateT(StateT, runStateT))
import Data.Array.Unboxed qualified as A
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (foldl', inits, sortBy, tails, mapAccumL)
import Data.Map (Map)
import Data.Map.Strict qualified as SMap
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH (DecsQ)

-- | Count the number of elements in a foldable value that satisfy a predicate.
count :: (Foldable f, Eq a) => a -> f a -> Int
count = countBy . (==)

-- | Count the number of elements in a foldable value that satisfy a predicate.
countBy :: Foldable f => (a -> Bool) -> f a -> Int
countBy p = foldl' (\acc x -> if p x then acc+1 else acc) 0

-- | Return true when the whole list is comprised of equal elements.
--
-- >>> same [1,1,1]
-- True
-- >>> same []
-- True
-- >>> same [1]
-- True
-- >>> same [1,1,2]
-- False
same :: Foldable t => Eq a => t a -> Bool
same x =
  case toList x of
    []   -> True
    y:ys -> all (y ==) ys

-- | Returns a list of ways to select an element from a list without
-- replacement.
--
-- >>> pickOne []
-- []
-- >>> pickOne [1]
-- [(1,[])]
-- >>> pickOne [1,2,3]
-- [(1,[2,3]),(2,[1,3]),(3,[1,2])]
pickOne :: [a] -> [(a, [a])]
pickOne xs = [ (x, l++r) | (l,x:r) <- zip (inits xs) (tails xs) ]

-- | Implementation of 'Data.List.nub' that uses 'Ord' for efficiency.
ordNub :: Ord a => [a] -> [a]
ordNub xs = foldr f (const []) xs Set.empty
  where
    f x rec seen =
      case rec <$> Set.alterF (\old -> (old, True)) x seen of
        (True,  ys) -> ys
        (False, ys) -> x : ys

-- | Compute the minimum element of a list or return Nothing if it is empty.
--
-- >>> minimumMaybe []
-- Nothing
-- >>> minimumMaybe [2,1,3]
-- Just 1
minimumMaybe :: Ord a => [a] -> Maybe a
minimumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! minimum xs


-- | Compute the maximum element of a list or return Nothing if it is empty.
--
-- >>> maximumMaybe []
-- Nothing
-- >>> maximumMaybe [2,1,3]
-- Just 3
maximumMaybe :: Ord a => [a] -> Maybe a
maximumMaybe xs
  | null xs   = Nothing
  | otherwise = Just $! maximum xs

-- | Compute the number of occurrences of the elements in a given list.
--
-- >>> counts "bababc"
-- fromList [('a',2),('b',3),('c',1)]
counts :: (Foldable f, Ord a) => f a -> Map a Int
counts xs = SMap.fromListWith (+) [(x,1) | x <- toList xs]

-- | Compose a list of functions together
--
-- >>> compose [ (1:), (2:), (3:) ] []
-- [1,2,3]
compose :: [a -> a] -> a -> a
compose = foldr (.) id

-- | Split list into chunks. The last chunk might be incomplete.
--
-- >>> chunks 3 [1..9]
-- [[1,2,3],[4,5,6],[7,8,9]]
--
-- >>> chunks 3 [1..7]
-- [[1,2,3],[4,5,6],[7]]
--
-- >>> chunks 3 []
-- []
chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  case splitAt n xs of
    (a,b) -> a : chunks n b

-- | Löb's theorem
--
-- <https://github.com/quchen/articles/blob/master/loeb-moeb.md>
-- <https://en.wikipedia.org/wiki/L%C3%B6b%27s_theorem>
löb :: Functor f => f (f a -> a) -> f a
löb = möb fmap

-- | 'löb' generalized over 'fmap'
möb :: (((a -> b) -> b) -> c -> a) -> c -> a
möb f = \x -> let go = f ($ go) x in go

-- | Index an array returning 'Nothing' if the index is out of bounds.
arrIx :: (A.IArray a e, A.Ix i, Alternative f) => a i e -> i -> f e
arrIx a i
  | A.inRange (A.bounds a) i = pure $! a A.! i
  | otherwise = empty
{-# Inline arrIx #-}

-- | Apply a function @n@ times strictly.
times :: Int -> (a -> a) -> a -> a
times n f x
  | n <= 0    = x
  | otherwise = times (n-1) f $! f x

-- | Given a list of constraints such that each constraint identifies
-- a unique variable and the set of assignments it can have, this
-- computes assignments of those variables such that no two input
-- variables are assigned the same value.
--
-- >>> uniqueAssignment [Set.fromList "ab", Set.fromList "bc"]
-- ["ab","ac","bc"]
uniqueAssignment ::
  (Traversable t, Ord a) =>
  t (Set a) {- ^ element must map to one of the corresponding set members -} ->
  [t a]     {- ^ possible assignments -}
uniqueAssignment m =
  [ snd (mapAccumL (\(x:xs) _ -> (xs,x)) (IntMap.elems a) m)
  | a <- go IntMap.empty (zip [0..] (toList m))]
  where
    go :: Ord a => IntMap a -> [(Int, Set a)] -> [IntMap a]
    go a xs =
      case sortBy (comparing (Set.size . snd)) xs of
        [] -> [a]
        (k,vs):rest ->
          do v <- Set.toList vs
             go (IntMap.insert k v a) (fmap (Set.delete v) <$> rest)

-- | Convert a big-endian list of digits to a single number.
--
-- >>> fromDigits 10 [1,2,3,4]
-- 1234
--
-- >>> fromDigits 2 [12]
-- 12
--
-- >>> fromDigits 10 []
-- 0
fromDigits :: Integral a => a -> [a] -> a
fromDigits base
  | base < 2  = error "fromDigits: bad base"
  | otherwise = foldl' (\acc x -> acc * base + x) 0

-- | Convert a number to a list of digits in a given radix.
--
-- >>> toDigits 2 12
-- [1,1,0,0]
--
-- >>> toDigits 10 1234
-- [1,2,3,4]
--
-- >>> toDigits 10 0
-- []
toDigits :: Integral a => a -> a -> [a]
toDigits base x
  | base < 2  = error "toDigits: bad base"
  | x < 0     = error "toDigits: negative number"
  | otherwise = go [] x
  where
    go xs 0 = xs
    go xs n = case quotRem n base of
                (n', digit) -> go (digit:xs) n'

-- | Efficient exponentiation using an associative operator
--
-- >>> power (+) 1 10
-- 10
--
-- >>> power (*) 2 10
-- 1024 
power :: (a -> a -> a) -> a -> Integer -> a
power (#) one n
  | n < 1 = error ("power: bad argument " ++ show n)
  | otherwise = go n
  where
    double x = x # x
    go i
      | 1 == i    = one
      | even i    = double (go (i `quot` 2))
      | otherwise = double (go (i `quot` 2)) # one

scanlM :: (Traversable t, Monad m) => (b -> a -> m (c, a)) -> a -> t b -> m (t c, a)
scanlM f z t = runStateT (traverse (coerce f) t) z

-- | Helper for putting declarations into scope for future Template Haskell
-- expressions. In particular this gets used so that the format quasiquoter
-- can see data types that it might want to parse.
stageTH :: DecsQ
stageTH = pure []
