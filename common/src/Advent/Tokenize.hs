{-# Language TypeOperators, DefaultSignatures, EmptyCase, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ImportQualifiedPost, BlockArguments #-}
{-|
Module      : Advent.Tokenize
Description : Automation for replacing data with Ints
Copyright   : (c) Eric Mertens, 2022
License     : ISC
Maintainer  : emertens@gmail.com

>>> autoTokenize [("A", [("A",40,["B"])])] :: [(Int, [(Int, Integer, [Int])])]
[(0,[(0,40,[1])])]

>>> autoTokenize ("A","A","B","B") :: (Int, Int, String, Int)
(0,0,"B",1)

-}
module Advent.Tokenize where

import Control.Monad.Trans.State.Strict (State, evalState, state)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic(Rep, from, to), V1, U1(U1), K1(K1), M1(M1), type (:+:)(L1,R1), type (:*:)((:*:)))

newtype Tokenize a = Tokenize (State (Map String Int) a)
  deriving (Functor, Applicative, Monad)

tok :: String -> Tokenize Int
tok t = Tokenize $
    state \m ->
        case Map.lookup t m of
            Just i -> (i,m)
            Nothing -> i `seq` m' `seq` (i,m')
                where
                    i = Map.size m
                    m' = Map.insert t i m

runTokenize :: Tokenize a -> a
runTokenize (Tokenize x) = evalState x Map.empty

autoTokenize :: AutoToken a b => a -> b
autoTokenize = runTokenize . autoToken

class AutoToken a b where
    autoToken :: a -> Tokenize b
    default autoToken :: (Generic a, Generic b, GAutoToken (Rep a) (Rep b)) => a -> Tokenize b
    autoToken = genericAutoToken

instance {-# INCOHERENT #-} AutoToken a a where
    autoToken = pure

instance AutoToken String Int where
    autoToken = tok

instance AutoToken a b => AutoToken (Map String a) (IntMap b) where
    autoToken m = IntMap.fromList <$> autoToken (Map.assocs m)

instance (Ord b, AutoToken a b) => AutoToken (Set a) (Set b) where
    autoToken m = Set.fromList <$> autoToken (Set.toList m)

instance AutoToken (Set String) IntSet where
    autoToken m = IntSet.fromList <$> autoToken (Set.toList m)

instance AutoToken a b => AutoToken [a] [b] where
    autoToken = traverse autoToken

instance AutoToken a b => AutoToken (Maybe a) (Maybe b) where
    autoToken = traverse autoToken

instance (AutoToken a1 b1, AutoToken a2 b2) => AutoToken (a1,a2) (b1,b2)

instance (AutoToken a1 b1, AutoToken a2 b2, AutoToken a3 b3) =>
    AutoToken (a1,a2,a3) (b1,b2,b3)

instance (AutoToken a1 b1, AutoToken a2 b2, AutoToken a3 b3, AutoToken a4 b4) =>
    AutoToken (a1,a2,a3,a4) (b1,b2,b3,b4)


class GAutoToken f g where
    gautoToken :: f x -> Tokenize (g x)

instance GAutoToken f g => GAutoToken (M1 i c f) (M1 j d g) where
    gautoToken (M1 x) = M1 <$> gautoToken x

instance (GAutoToken f1 f2, GAutoToken g1 g2) =>
    GAutoToken (f1 :*: g1) (f2 :*: g2) where
    gautoToken (x :*: y) = (:*:) <$> gautoToken x <*> gautoToken y

instance (GAutoToken f1 f2, GAutoToken g1 g2) =>
    GAutoToken (f1 :+: g1) (f2 :+: g2) where
    gautoToken (L1 x) = L1 <$> gautoToken x
    gautoToken (R1 x) = R1 <$> gautoToken x

instance AutoToken a b => GAutoToken (K1 i a) (K1 i b) where
    gautoToken (K1 x) = K1 <$> autoToken x

instance GAutoToken U1 U1 where
    gautoToken _ = pure U1

instance GAutoToken V1 V1 where
    gautoToken v = case v of {}

genericAutoToken ::
    (Generic a, Generic b, GAutoToken (Rep a) (Rep b)) =>
    a -> Tokenize b
genericAutoToken x = to <$> gautoToken (from x)
{-# INLINE genericAutoToken #-}
