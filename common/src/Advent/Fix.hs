{-# Language ImportQualifiedPost #-}
{-|
Module      : Advent.SmallSet
Description : An efficient set representation for small integers.
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

A compact set type for when you have very few elements to track.

-}
module Advent.Fix where

import Data.Functor.Classes (showsPrec1, Show1)
import Data.Map (Map)
import Data.Map qualified as Map

-- | Fixed-point of a type
newtype Fix f = Fix { unFix :: f (Fix f) }

instance Show1 f => Show (Fix f) where
  showsPrec p (Fix x) = showParen (p >= 11)
      $ showString "Fix "
      . showsPrec1 11 x

-- | Generic fold
cata :: Functor t => (t a -> a) -> Fix t -> a
cata f (Fix x) = f (fmap (cata f) x)

-- | Generic monadic fold
cataM :: Monad m => Traversable t => (t a -> m a) -> Fix t -> m a
cataM f (Fix x) = f =<< traverse (cataM f) x

-- | Generic unfold
ana :: Functor f => (a -> f a) -> (a -> Fix f)
ana f = Fix . fmap (ana f) . f

-- | Convert a map of values parameterized by names into a recursively
-- defined datatype.
anaFromMap ::
  (Ord k, Functor f) =>
  Map k (f k) {- ^ entries by name                          -} ->
  k           {- ^ root name                                -} ->
  Fix f       {- ^ root node with keys recursively resolved -}
anaFromMap m = ana (m Map.!)
