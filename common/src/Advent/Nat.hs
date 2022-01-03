{-|
Module      : Advent.Nat
Description : Type level nats as successor and zero
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Nat where

-- | Natural numbers (used for type index)
data Nat
  = Z     -- ^ zero
  | S Nat -- ^ successor
