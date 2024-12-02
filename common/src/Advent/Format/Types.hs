{-|
Module      : Advent.Format.Types
Description : Types for the parser AST
Copyright   : (c) Eric Mertens, 2018-2021
License     : ISC
Maintainer  : emertens@gmail.com

-}

module Advent.Format.Types where

data Token
  = TOpenGroup
  | TCloseGroup
  | TAnyChar
  | TAnyLetter
  | TAnyWord
  | TUnsignedInteger
  | TSignedInteger
  | THexInteger
  | TUnsignedInt
  | TSignedInt
  | THexInt
  | TMany
  | TSome
  | TSepBy
  | TAlt
  | TAt String
  | TBang
  | TLiteral Char
  deriving (Eq, Ord, Show, Read)

data Format
  -- repetitions
  = Many  Format
  | Some  Format
  | SepBy Format Format
  -- combinations
  | Alt Format Format
  | Follow [Format]
  -- return matched string
  | Gather Format
  | Named String
  -- explicit grouping to allow subtuples
  | Group Format
  -- primitives
  | Literal String
  | UnsignedInteger
  | SignedInteger
  | HexInteger
  | UnsignedInt
  | SignedInt
  | HexInt
  | Word
  | Char
  | Letter
  deriving Show
