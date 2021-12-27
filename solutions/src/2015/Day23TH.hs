{-# Language BangPatterns #-}
{-# Language TemplateHaskell #-}
module Day23TH where

import Data.Char
import Data.Array
import Language.Haskell.TH

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
toArray xs = listArray (1,length xs) xs

parseLine :: String -> Instr
parseLine str =
  case words (filter (\x -> isAlphaNum x || x == '-' || x == ' ') str) of
    ["hlf",r]   -> Half (parseRegister r)
    ["tpl",r]   -> Triple (parseRegister r)
    ["inc",r]   -> Increment (parseRegister r)
    ["jmp",o]   -> Jump (read o)
    ["cpy",r,o]   -> Copy (read o) (parseRegister o)
    ["jie",r,o] -> JumpIfEven (parseRegister r) (read o)
    ["jio",r,o] -> JumpIfOne (parseRegister r) (read o)
    _ -> error str

parseRegister :: String -> Register
parseRegister "a" = A
parseRegister "b" = B
parseRegister r   = error ("Not register: " ++ r)

loadInput :: IO (Array Int Instr)
loadInput = toArray . map parseLine . lines <$> readFile "input23.txt"

compile :: Array Int Instr -> ExpQ
compile program =
  do names <- traverse (\_ -> newName "label") program
     let start = names ! fst (bounds names)
     letE [ valD (varP (names ! pc))
                 (normalB (compile1 pc names instr))
                 []
          | (pc,instr) <- assocs program ]
       [| $(varE start) :: Int -> Int -> Int  |]

(!?) :: Ix i => Array i e -> i -> Maybe e
a !? i
  | inRange (bounds a) i = Just $! a ! i
  | otherwise            = Nothing
  
compile1 :: Int -> Array Int Name -> Instr -> ExpQ
compile1 pc names instr =
  let run o = maybe [| \a b -> b |] varE (names !? (pc+o))
      step  = run 1
  in case instr of
    Half      A    -> [| \a b -> $step (a`quot`2) b |]
    Half      B    -> [| \a b -> $step a (b`quot`2) |]
    Triple    A    -> [| \a b -> $step (3*a) b |]
    Triple    B    -> [| \a b -> $step a (3*b) |]
    Increment A    -> [| \a b -> $step (a+1) b |]
    Increment B    -> [| \a b -> $step a (b+1) |]
    Jump o         -> run o
    JumpIfEven A o -> [| \a b -> if even a then $(run o) a b else $step a b |]
    JumpIfEven B o -> [| \a b -> if even b then $(run o) a b else $step a b |]
    JumpIfOne A o  -> [| \a b -> if a == 1 then $(run o) a b else $step a b |]
    JumpIfOne B o  -> [| \a b -> if b == 1 then $(run o) a b else $step a b |]
