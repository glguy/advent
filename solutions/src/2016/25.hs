{-# Language ViewPatterns, ImportQualifiedPost, MonoLocalBinds, TemplateHaskell, LambdaCase #-}
module Main where

import Advent
import AsmProg
import Control.Applicative
import Control.Lens
import Control.Monad.Trans.State
import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Text.ParserCombinators.ReadP

data Progress = NeedOne | NeedZero

data Machine = Machine
  { _machRegisters :: !Registers
  , _machProgress  :: !Progress
  , _machTargets   :: !(Set (Int,Registers))
  }

makeLenses ''Machine

instance HasRegisters Machine where
  reg r = machRegisters . reg r
  {-# INLINE reg #-}

main :: IO ()
main =
 do program <- Vector.fromList . map parseLine <$> getInputLines 25
    print $ find (execute program) [1..]

data Inst
  = Copy Value Register
  | Inc Register
  | Dec Register
  | Jnz Value Value
  | Out Value
 deriving Show

parseLine :: String -> Inst
parseLine (readP_to_S pInst -> [(x,_)]) = x

pInst :: ReadP Inst
pInst =
  Copy <$ string "cpy " <*> pValue <* char ' ' <*> pReg <|>
  Jnz  <$ string "jnz " <*> pValue <* char ' ' <*> pValue <|>
  Inc  <$ string "inc " <*> pReg <|>
  Dec  <$ string "dec " <*> pReg <|>
  Out  <$ string "out " <*> pValue

execute :: Vector Inst -> Int -> Bool
execute program a =
  evalState theMain (Machine zeroRegisters NeedZero mempty)
  where
    theMain = do reg A .= a
                 goto 0

    step pc = \case
      Out o ->
         do v <- rval o
            progress <- use machProgress
            case (progress, v) of
              (NeedOne,  1) ->
                  do machProgress .= NeedZero
                     goto (pc+1)

              (NeedZero, 0) ->
                  do registers <- use machRegisters
                     targets   <- use machTargets
                     let now = (pc,registers)
                     if Set.member now targets then
                       return True
                     else
                       do machTargets . contains now .= True
                          machProgress               .= NeedOne
                          goto (pc+1)

              _ -> return False

      Copy i o -> do reg o <~ rval i
                     goto (pc+1)

      Inc r    -> do reg r += 1
                     goto (pc+1)

      Dec r    -> do reg r -= 1
                     goto (pc+1)

      Jnz i o  -> do v  <- rval i
                     o' <- rval o
                     let pcOff = if v == 0 then 1 else o'
                     goto (pc+pcOff)

    goto pc =
      case program Vector.!? pc of
        Nothing -> return False
        Just o  -> step pc o
