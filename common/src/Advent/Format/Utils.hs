{-|
Module      : Advent.Format.Utils
Description : Queries and transforms for the format AST
Copyright   : (c) Eric Mertens, 2018-2021
License     : ISC
Maintainer  : emertens@gmail.com

-}
module Advent.Format.Utils where

import Advent.Format.Enum (enumCases)
import Advent.Format.Types (Format(..))
import Control.Monad (when)
import Data.Char (isUpper)
import Language.Haskell.TH (Q)

interesting :: Format -> Bool
interesting s =
  case s of
    Many x              -> interesting x
    Some x              -> interesting x
    SepBy x _           -> interesting x
    Group x             -> interesting x
    Alt x y             -> interesting x || interesting y
    Follow xs           -> any interesting xs
    UnsignedInteger     -> True
    SignedInteger       -> True
    HexInteger          -> True
    UnsignedInt         -> True
    SignedInt           -> True
    HexInt              -> True
    Word                -> True
    Char                -> True
    Letter              -> True
    Gather{}            -> True
    Named{}             -> True
    Literal{}           -> False

acceptsEmpty :: Format -> Q Bool
acceptsEmpty fmt =
  case fmt of
    Many _              -> pure True
    Some x              -> acceptsEmpty x
    SepBy _ _           -> pure True
    Alt x y             -> anyM acceptsEmpty [x, y]
    Follow xs           -> allM acceptsEmpty xs
    UnsignedInteger     -> pure False
    SignedInteger       -> pure False
    HexInteger          -> pure False
    UnsignedInt         -> pure False
    SignedInt           -> pure False
    HexInt              -> pure False
    Word                -> pure False
    Char                -> pure False
    Letter              -> pure False
    Gather x            -> acceptsEmpty x
    Group x             -> acceptsEmpty x
    Literal x           -> pure (null x)
    Named name
      | isUpper (head name) ->
         do cases <- enumCases name
            pure (any (null . snd) cases)
      | otherwise -> pure False

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ [] = pure False
anyM f (x : xs) = f x >>= \b -> if b then pure True else anyM f xs

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM f (x : xs) = f x >>= \b -> if b then allM f xs else pure False

simplify :: Format -> Format
simplify (Follow xs) = Follow (foldr simplifyFollow [] xs)
simplify (Alt x y)   = Alt (simplify x) (simplify y)
simplify (Group x)   = Group (simplify x)
simplify (SepBy x y) = SepBy (simplify x) (simplify y)
simplify (Gather x)  = Gather (simplify x)
simplify (Many x)    = Many (simplify x)
simplify (Some x)    = Some (simplify x)
simplify x           = x

simplifyFollow :: Format -> [Format] -> [Format]
simplifyFollow (Follow xs) z = foldr simplifyFollow z xs
simplifyFollow (Literal a) (Literal b : c) = Literal (a++b) : c
simplifyFollow x z = simplify x : z
