{-# Language BlockArguments, ViewPatterns #-}
{-|
Module      : Advent.ReadS
Description : Newtype for parsing with ReadS
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

Make it easier to use ReadS.

-}
module Advent.ReadS where

import Control.Applicative ( Alternative((<|>), empty) )
import Control.Monad ( ap, liftM )

-- | Wrapper for 'ReadS'
newtype P a = P (ReadS a)

-- | Parse a string or throw an error
runP :: P a -> String -> a
runP (P f) (f -> (x,_):_) = x
runP _ x = error ("failed to parse: " ++ x)

-- | Match a specific string token and return it.
tok :: String -> P String
tok t = do u <- P lex; if t == u then pure u else empty

instance Functor P where
    fmap = liftM

instance Applicative P where
    (<*>) = ap
    pure x = P \s -> [(x,s)]

instance Monad P where
    P m >>= f = P \s -> do (x,s) <- m s; case f x of P g -> g s

instance Alternative P where
    P x <|> P y = P \s -> x s <|> y s
    empty = P \_ -> []
