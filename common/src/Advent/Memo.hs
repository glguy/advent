{-|
Module      : Advent.Memo
Description : Memoization functions
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

Re-exported MemoTrie operations and extended
arity memoization.

-}
module Advent.Memo (
  HasTrie,
  memo, memo2, memo3, memo4, memo5, memo6,
  ) where

import Data.MemoTrie (HasTrie, memo, memo2, memo3, mup)

-- | Memoize a quaternary function on successive arguments.
-- Take care to exploit any partial evaluation.
memo4 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d) =>
  (a -> b -> c -> d -> e) ->
  (a -> b -> c -> d -> e)
memo4 = mup memo3

-- | Memoize a quaternary function on successive arguments.
-- Take care to exploit any partial evaluation.
memo5 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d, HasTrie e) =>
  (a -> b -> c -> d -> e -> f) ->
  (a -> b -> c -> d -> e -> f)
memo5 = mup memo4

-- | Memoize a quaternary function on successive arguments.
-- Take care to exploit any partial evaluation.
memo6 ::
  (HasTrie a, HasTrie b, HasTrie c, HasTrie d, HasTrie e, HasTrie f) =>
  (a -> b -> c -> d -> e -> f -> g) ->
  (a -> b -> c -> d -> e -> f -> g)
memo6 = mup memo5
