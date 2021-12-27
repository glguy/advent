{-# Language ImportQualifiedPost, OverloadedStrings #-}
{-|
Module      : Advent
Description : Solution helper library
Copyright   : (c) Eric Mertens, 2018-2021
License     : ISC
Maintainer  : emertens@gmail.com

This module re-exports the most commonly used modules.

* "Advent.Prelude" is full of useful helper functions
* "Advent.Input" provides quick access to inputs in a few formats
* "Advent.Format" provides a quasi-quoter for making input parsers 

-}
module Advent (
  module Advent.Prelude,
  module Advent.Input,
  module Advent.Format,
  ) where

import Advent.Prelude
import Advent.Input
import Advent.Format
