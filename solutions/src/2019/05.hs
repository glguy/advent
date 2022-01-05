{-# LANGUAGE QuasiQuotes #-}
{-|
Module      : Main
Description : Day 5 solution
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2019/day/5>

This task expands the virtual machine defined in day 2
adding jumps, conditionals, inputs, and outputs.

This solution works with the following passes:

  1. Parse input text file into a list of numbers
  2. Execute op codes to extract the input/output "effects"
  3. Evaluate the effect as a function from a list of inputs to list of outputs
  4. Apply the function to a single input and find the last output.

>>> intcodeToList [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9] <$> [[0],[10]]
[[0],[1]]

>>> intcodeToList [3,3,1105,-1,9,1101,0,0,12,4,12,99,1] <$> [[0],[10]]
[[0],[1]]

>>> :{
>>> intcodeToList
>>>      [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
>>>       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
>>>       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
>>>   <$> [[7],[8],[9]]
>>> :}
[[999],[1000],[1001]]

-}
module Main (main) where

import Advent (format)
import Intcode (intcodeToList)

-- | >>> :main
-- 15508323
-- 9006327
main :: IO ()
main =
  do inp <- [format|2019 5 %d&,%n|]
     let go i = print (last (intcodeToList inp [i]))
     go 1
     go 5
