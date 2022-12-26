{-# Language TypeFamilies, RankNTypes, UndecidableInstances, TypeOperators, ImportQualifiedPost, DataKinds #-}
{-|
Module      : Advent.Nat
Description : Type level nats as successor and zero
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Nat where

import GHC.TypeNats qualified as T

-- | Natural numbers (used for type index)
data Nat
  = Z     -- ^ zero
  | S Nat -- ^ successor

-- | Covert from GHC type literal syntax to an inductively defined natural
type family FromNatural (n :: T.Natural) :: Nat where
  FromNatural 0 = 'Z
  FromNatural n = 'S (FromNatural (n T.- 1))

class UnfoldNat n where
  unfoldNat :: f 'Z -> (forall m. f m -> f ('S m)) -> f n

instance UnfoldNat 'Z where
  unfoldNat z _ = z

instance UnfoldNat n => UnfoldNat ('S n) where
  unfoldNat z s = s (unfoldNat z s)
