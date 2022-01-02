module Advent.Nat where

-- | Natural numbers (used for type index)
data Nat
  = Z     -- ^ zero
  | S Nat -- ^ successor
