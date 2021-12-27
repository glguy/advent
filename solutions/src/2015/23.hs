{-# Language BangPatterns #-}
module Main (main) where

import Advent ( getInputLines, arrIx )
import Data.Char ( isAlphaNum )
import Data.Array ( Array, listArray )

main :: IO ()
main =
  do pgm <- loadInput
     print (program pgm 0 0 0)
     print (program pgm 0 1 0)

data Instr
  = Half Register
  | Triple Register
  | Increment Register
  | Copy Int Register
  | Jump Int
  | JumpIfEven Register Int
  | JumpIfOne Register Int
  deriving Show

data Register = A | B
  deriving Show

toArray :: [a] -> Array Int a
toArray xs = listArray (0,length xs - 1) xs

parseLine :: String -> Instr
parseLine str =
  case words (filter (\x -> isAlphaNum x || x == '-' || x == ' ') str) of
    ["hlf",r]   -> Half (parseRegister r)
    ["tpl",r]   -> Triple (parseRegister r)
    ["inc",r]   -> Increment (parseRegister r)
    ["jmp",o]   -> Jump (read o)
    ["cpy",r,o] -> Copy (read o) (parseRegister o)
    ["jie",r,o] -> JumpIfEven (parseRegister r) (read o)
    ["jio",r,o] -> JumpIfOne (parseRegister r) (read o)
    _ -> error str

parseRegister :: String -> Register
parseRegister "a" = A
parseRegister "b" = B
parseRegister r   = error ("Not register: " ++ r)

loadInput :: IO (Array Int Instr)
loadInput = toArray . map parseLine <$> getInputLines 23

program :: Array Int Instr -> Int -> Int -> Int -> Int
program pgm pc !a !b =
  let step = program pgm (pc+1) in
  case arrIx pgm pc of
    Nothing -> b
    Just instr ->
      case instr of
        Half      A    -> step (a`quot`2) b
        Half      B    -> step a (b`quot`2)
        Triple    A    -> step (3*a) b
        Triple    B    -> step a (3*b)
        Increment A    -> step (a+1) b
        Increment B    -> step a (b+1)
        Jump o         -> program pgm (pc + o) a b
        JumpIfEven A o -> if even a then program pgm (pc+o) a b else step a b
        JumpIfEven B o -> if even b then program pgm (pc+o) a b else step a b
        JumpIfOne A o  -> if a == 1 then program pgm (pc+o) a b else step a b
        JumpIfOne B o  -> if b == 1 then program pgm (pc+o) a b else step a b
