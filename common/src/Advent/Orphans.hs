{-# Language TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module      : Advent.Orphans
Description : Orphan instances
Copyright   : 2022 Eric Mertens
License     : ISC
Maintainer  : emertens@gmail.com

Orphan instances missing from other packages.

-}
module Advent.Orphans where

import Data.String ( IsString(..) )
import Text.ParserCombinators.ReadP ( ReadP, string )

instance a ~ String => IsString (ReadP a) where
  fromString = string