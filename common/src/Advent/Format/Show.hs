{-|
Module      : Advent.Format.Show
Description : Rendering for the parser DSL
Copyright   : (c) Eric Mertens, 2018-2021
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Advent.Format.Show where

import Advent.Format.Types (Format(..), Token(..))

-- | Render a parsed format string back to the input syntax.
showFormat :: Int {- ^ surrounding precedence -} -> Format -> ShowS
showFormat p fmt =
  case fmt of
    Many x              -> showFormat 3 x . showChar '*'
    Some x              -> showFormat 3 x . showChar '+'
    Gather x            -> showFormat 3 x . showChar '!'
    SepBy x y           -> showFormat 3 x . showChar '&' . showFormat 3 y
    Alt x y             -> showParen (p > 1) $ showFormat 1 x . showChar '|' . showFormat 2 y
    Follow xs           -> showParen (p > 2) $ \z -> foldr (showFormat 3) z xs
    Group x             -> showFormat 3 x
    UnsignedInteger     -> showString "%lu"
    SignedInteger       -> showString "%ld"
    HexInteger          -> showString "%lx"
    UnsignedInt         -> showString "%u"
    SignedInt           -> showString "%d"
    HexInt              -> showString "%x"
    Word                -> showString "%s"
    Char                -> showString "%c"
    Letter              -> showString "%a"
    Named n             -> showChar '@' . showString n
    Literal x           -> flip (foldr showLiteral) x

-- | Render a literal character match back to input syntax.
showLiteral :: Char -> ShowS
showLiteral x
  | x == '\n' = showString "%n"
  | x `elem` "()&!*+%@" = showChar '%' . showChar x
  | otherwise = showChar x

showToken :: Token -> String
showToken t =
  case t of
    TOpenGroup        -> "("
    TCloseGroup       -> ")"
    TAnyChar          -> "%c"
    TAnyLetter        -> "%a"
    TAnyWord          -> "%s"
    TUnsignedInteger  -> "%lu"
    TSignedInteger    -> "%ld"
    THexInteger       -> "%lx"
    TUnsignedInt      -> "%u"
    TSignedInt        -> "%d"
    THexInt           -> "%x"
    TMany             -> "*"
    TSome             -> "+"
    TSepBy            -> "&"
    TAlt              -> "|"
    TAt x             -> "@" ++ x
    TBang             -> "!"
    TLiteral c        -> showLiteral c ""