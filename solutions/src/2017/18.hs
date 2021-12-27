{-# Language ImportQualifiedPost, ViewPatterns #-}
{-|
Module      : Main
Description : Day 18 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2017/day/18>

Day 18 defines a simple programming language with arithmetic operations
and asynchronous communication. Our task will be to analyze the behavior
of the send and receive commands performed by these kinds of programs.

This implementation uses the following passes to transform the input
program into a high-level interpretation of the effects of the program
from which we can then easily answer the questions posed.

1. Get input file with 'getInput'
2. Parse the input with 'parser'
3. Compute effects with 'interpreter'
4. Analyze the effects with 'part1' and 'part2'

>>> :main
Just 2951
7366
-}
module Main
  (
  -- * Main drivers
    main
  , part1
  , part2
  , feed

  -- * Interpreter
  -- $interp
  , Effect(..)
  , interpreter

  -- * Parser
  -- $parser
  , Instruction(..)
  , Expression(..)
  , Register(..)
  , instruction
  , register
  , expression
  ) where

import Advent.Input ( getInputLines )
import Control.Applicative ((<|>))
import Data.Char (isAlpha, isDigit)
import Data.Map qualified as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Vector qualified as V
import Text.ParserCombinators.ReadP

-- | Print the solution to both parts of the puzzle. Input file can be
-- overridden via command-line argument.
main :: IO ()
main =
  do pgm <- map parseInstruction <$> getInputLines 18
     let start = interpreter pgm
     print (part1 start)
     print (part2 start)

-- | Compute the last send command that precedes a non-zero receive command.
--
-- >>> :{
-- let Right pgm = Advent.parseLines instruction
--                     "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\n\
--                     \rcv a\njgz a -1\nset a 1\njgz a -2\n"
--   in part1 (interpreter pgm)
-- :}
-- Just 4
part1 ::
  (Integer -> Effect) {- ^ program ID to effect         -} ->
  Maybe Integer       {- ^ last non-zero snd before rcv -}
part1 start = go Nothing (start 0)
  where
    go :: Maybe Integer -> Effect -> Maybe Integer
    go _ (Send x p)    = go (Just x) p -- remember last send
    go s (Receive 0 p) = go s (p 0)    -- ignore rcv 0, put 0 back
    go s (Receive _ _) = s             -- non-zero rcv, we're done
    go _ Halt          = Nothing       -- never found the non-zero rcv!


-- | Run two programs concurrently and count how many sends the second program
-- executes once both programs are blocked.
--
-- >>> :{
-- let Right pgm = Advent.parseLines instruction
--                      "snd 1\nsnd 2\nsnd p\nrcv a\nrcv b\nrcv c\nrcv d\n"
--   in part2 (interpreter pgm)
-- :}
-- 3
part2 ::
  (Integer -> Effect) {- ^ program ID to effect -} ->
  Int                 {- ^ sends from program 1 -}
part2 start = go (start 0) (start 1) 0
  where
    go :: Effect -> Effect -> Int -> Int
    go (Send o p0) p1 ctr = go p0 (feed o p1) ctr
    go p0 (Send o p1) ctr = go (feed o p0) p1 (ctr+1)
    go _ _            ctr = ctr


-- | Provide the given 'Integer' argument to the first 'Receive' command in a
-- given effect sequence.
feed :: Integer -> Effect -> Effect
feed i (Send o p)       = Send o (feed i p)
feed i (Receive _ k)    = k i
feed _ Halt             = Halt

------------------------------------------------------------------------

-- $interp
-- The Interpreter transforms a program from the world of instructions,
-- registers, and program counters into only the effects of interpreting
-- those programs. We'll be able to process these effects in order to answer
-- the questions posed in part 1 and part 2 of this task.

-- | Observable program execution effects
data Effect
  = Halt                -- ^ Execution complete
  | Send Integer Effect -- ^ Send integer, continue
  | Receive Integer (Integer -> Effect)
  -- ^ Receive with original register value and continuation taking new value


-- | Compute the effect of executing a program starting at the first instruction
-- using the given map as the initial set of registers.
interpreter ::
  [Instruction] {- ^ instructions   -} ->
  Integer       {- ^ program ID     -} ->
  Effect        {- ^ program effect -}
interpreter cmds pid = go 0 initialRegs
  where
    v = V.fromList cmds
    initialRegs = Map.singleton (Register 'p') pid

    go ::
      Int                  {- ^ program counter -} ->
      Map Register Integer {- ^ registers       -} ->
      Effect               {- ^ program effect  -}
    go pc regs =
      case v V.!? pc of
        Nothing        -> Halt
        Just (Snd e  ) -> Send    (val e) (go (pc+1) regs)
        Just (Rcv r  ) -> Receive (reg r) (go (pc+1) . set r)
        Just (Set r e) -> go (pc+1) (set r (val e))
        Just (Add r e) -> go (pc+1) (upd r (val e +))
        Just (Mul r e) -> go (pc+1) (upd r (val e *))
        Just (Mod r e) -> go (pc+1) (upd r (`rem` val e))
        Just (Jgz x y) -> go (pc+o) regs
          where o | val x > 0 = fromIntegral (val y)
                  | otherwise = 1
      where
        val (RegisterExpression r) = reg r                  -- evaluate register
        val (IntegerExpression  i) = i                      -- evaluate literal
        reg r   = Map.findWithDefault 0 r regs              -- lookup register
        set r x = Map.insert r x regs                       -- assign register
        upd r f = Map.alter (Just . f . fromMaybe 0) r regs -- update register

------------------------------------------------------------------------

-- $parser
-- The language defined by this problem is particularly simple and so is
-- its parser. Each instruction can be found on its own line, and tokens
-- in the language are separated by whitespace. Each instruction has one
-- or two operands. Some of these operands need to be register names while
-- others can be an expression composed of either an integer literal or
-- a register name.

-- | Register names: single letters
newtype Register = Register Char
  deriving (Read, Show, Eq, Ord)

-- | Expressions are either integer literals or register values
data Expression
  = RegisterExpression Register -- ^ read from register
  | IntegerExpression  Integer  -- ^ constant integer
  deriving (Read, Show)

-- | Program instruction
data Instruction
  = Snd Expression            -- ^ @snd e@: send @e@
  | Rcv Register              -- ^ @rcv r@: receive to @r@
  | Set Register Expression   -- ^ @set r e@: @r=e@
  | Add Register Expression   -- ^ @add r e@: @r=r+e@
  | Mul Register Expression   -- ^ @mul r e@: @r=r*e@
  | Mod Register Expression   -- ^ @mod r e@: @r=r%e@
  | Jgz Expression Expression -- ^ @jgz t o@: @if t>0 then pc+=o@
  deriving (Read, Show)

parseInstruction :: String -> Instruction
parseInstruction (readP_to_S instruction -> [(x,_)]) = x
parseInstruction x = error ("bad instruction: " ++ x)

instruction :: ReadP Instruction
instruction =
  Snd <$ string "snd " <*> expression                            <|>
  Rcv <$ string "rcv " <*> register                              <|>
  Set <$ string "set " <*> register   <* char ' ' <*> expression <|>
  Add <$ string "add " <*> register   <* char ' ' <*> expression <|>
  Mul <$ string "mul " <*> register   <* char ' ' <*> expression <|>
  Mod <$ string "mod " <*> register   <* char ' ' <*> expression <|>
  Jgz <$ string "jgz " <*> expression <* char ' ' <*> expression

expression :: ReadP Expression
expression =
  RegisterExpression <$> register <|>
  IntegerExpression  <$> number

register :: ReadP Register
register = Register <$> satisfy isAlpha

number :: ReadP Integer
number = read <$> ((++) <$> option "" (string "-") <*> munch1 isDigit)
