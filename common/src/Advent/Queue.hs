{-|
Module      : Advent.Queue
Description : Banker's queue implementation
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

-}
{-# Language PatternSynonyms, ViewPatterns #-}
{-# Options_GHC -Wno-name-shadowing #-}
module Advent.Queue (Queue(Empty, (:<|)), (|>), singleton, fromList, snoc, pop, appendList) where

import Data.Foldable (Foldable(..))
import Data.Monoid   (Dual(..))
import Data.Coerce   (coerce)

-- | FIFO Queue implementation
data Queue a = Queue [a] [a] !Int

{-# COMPLETE (:<|), Empty #-}

-- | Empty queue
--
-- >>> Empty :: Queue Char
-- fromList ""
pattern Empty :: Queue a
pattern Empty <- Queue [] _ _
  where
    Empty = Queue [] [] 0

-- | Pattern for 'pop'
--
-- >>> let x :<| xs = fromList "abc" in (x, xs)
-- ('a',fromList "bc")
pattern (:<|) :: a -> Queue a -> Queue a
pattern x :<| xs <- (pop -> Just (x, xs))

-- | Add an element to the end of a queue. See: 'snoc'
--
-- >>> fromList "abc" |> 'z'
-- fromList "abcz"
(|>) :: Queue a -> a -> Queue a
q |> x = snoc x q
{-# INLINE (|>) #-}

-- | Fold over elements in the order they would be returned by pop
--
-- >>> toList (fromList "abc")
-- "abc"
instance Foldable Queue where
  null      (Queue l _ _) = null l
  length    (Queue l r _) = length l + length r
  elem x    (Queue l r _) = elem x l || elem x r
  sum       (Queue l r _) = sum l + sum r
  foldMap _ (Queue [] _ _) = mempty
  foldMap f (Queue (x:l) r 0) = f x <> rot l r
    where
      rot []     (y:_ ) = f y
      rot (x:xs) (y:ys) = f x <> rot xs ys <> f y
  foldMap f (Queue (x:l) r i) = f x <> foldMap f (Queue l r (i-1))


-- | Renders using 'fromList' syntax.
--
-- >>> show (fromList "example")
-- "fromList \"example\""
instance Show a => Show (Queue a) where
  showsPrec p q
    = showParen (p >= 11)
    $ showString "fromList "
    . shows (toList q)

-- >>> read "fromList \"example\"" :: Queue Char
-- fromList "example"
instance Read a => Read (Queue a) where
  readsPrec prec
    = readParen (prec >= 11) $ \str ->
      do ("fromList", str) <- lex str
         (xs,         str) <- reads str
         return (fromList xs, str)

-- | Construct a queue from a single element.
--
-- >>> singleton 'a'
-- fromList "a"
singleton :: a -> Queue a
singleton x = Queue [x] [] 1

-- | Construct a queue from a list. The head of the list will
-- be the first element returned by 'pop'
fromList :: [a] -> Queue a
fromList xs = Queue xs [] (length xs)

-- | Append many items onto a queue. The items will pop from the queue
-- in the same order as they are in the given list.
--
-- >>> appendList "abc" (fromList "xyz")
-- fromList "xyzabc"
appendList :: [a] -> Queue a -> Queue a
appendList xs q = foldl' (|>) q xs

-- | Remove an element from the front of a queue and a new queue
-- without that element.
--
-- >>> pop (fromList "abc")
-- Just ('a',fromList "bc")
pop :: Queue a -> Maybe (a, Queue a)
pop (Queue (x:f) r s) = Just (x, exec f r s)
pop _                 = Nothing

-- | Add a new element to the end of a queue.
--
-- >>> snoc 'z' (fromList "abc")
-- fromList "abcz"
snoc :: a -> Queue a -> Queue a
snoc x (Queue f r s) = exec f (x:r) s

exec :: [a] -> [a] -> Int -> Queue a
exec f r 0    = fromList (rotate f r [])
exec f r i = Queue f r (i-1)

rotate []     (y:_ ) a = y : a
rotate (x:xs) (y:ys) a = x : rotate xs ys (y:a)
rotate _      _      _ = error "Advent.Queue.rotate: panic"
