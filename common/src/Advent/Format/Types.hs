{-# Language BlockArguments #-}
module Advent.Format.Types where

import Language.Haskell.TH
import Data.Traversable (for)
import Data.List (stripPrefix)
import Control.Monad (when)
import Data.Char (isUpper)

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
  | Follow Format Format
  | Empty
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

interesting :: Format -> Bool
interesting s =
  case s of
    Many x              -> interesting x
    Some x              -> interesting x
    SepBy x _           -> interesting x
    Group x             -> interesting x
    Alt x y             -> interesting x || interesting y
    Follow x y          -> interesting x || interesting y
    Empty               -> False
    UnsignedInteger     -> True
    SignedInteger       -> True
    HexInteger          -> True
    UnsignedInt         -> True
    SignedInt           -> True
    HexInt              -> True
    Word                -> True
    Char                -> True
    Letter              -> True
    Gather{}            -> True
    Named{}             -> True
    Literal{}           -> False

acceptsEmpty :: Format -> Q Bool
acceptsEmpty fmt =
  case fmt of
    Many _              -> pure True
    Some x              -> acceptsEmpty x
    SepBy _ _           -> pure True
    Alt x y             -> orM  (acceptsEmpty x) (acceptsEmpty y)
    Follow x y          -> andM (acceptsEmpty x) (acceptsEmpty y)
    Empty               -> pure True
    UnsignedInteger     -> pure False
    SignedInteger       -> pure False
    HexInteger          -> pure False
    UnsignedInt         -> pure False
    SignedInt           -> pure False
    HexInt              -> pure False
    Word                -> pure False
    Char                -> pure False
    Letter              -> pure False
    Gather x            -> acceptsEmpty x
    Group x             -> acceptsEmpty x
    Literal x           -> pure (null x)
    Named name
      | isUpper (head name) ->
         do cases <- enumCases name
            pure (any (null . snd) cases)
      | otherwise -> pure False


whenM :: Monad m => m Bool -> m () -> m ()
whenM pm m = pm >>= \p -> when p m

andM :: Monad m => m Bool -> m Bool -> m Bool
andM x y = x >>= \b -> if b then y else pure False

orM :: Monad m => m Bool -> m Bool -> m Bool
orM  x y = x >>= \b -> if b then pure True else y

-- | Render a parsed format string back to the input syntax.
showFormat :: Int {- ^ surrounding precedence -} -> Format -> ShowS
showFormat p fmt =
  case fmt of
    Many x              -> showFormat 3 x . showChar '*'
    Some x              -> showFormat 3 x . showChar '+'
    Gather x            -> showFormat 3 x . showChar '!'
    SepBy x y           -> showFormat 3 x . showChar '&' . showFormat 3 y
    Alt x y             -> showParen (p > 1) $ showFormat 1 x . showChar '|' . showFormat 2 y
    Follow x y          -> showParen (p > 2) $ showFormat 2 x . showFormat 3 y
    Empty               -> showParen (p > 2) id
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

follow :: Format -> Format -> Format
follow Empty x = x
follow x Empty = x
follow x y = Follow x y

enumCases :: String -> Q [(Name,String)]
enumCases nameStr =
 do tyName <- maybe (fail ("Failed to find type named " ++ show nameStr)) pure
           =<< lookupTypeName nameStr

    info <- reify tyName
    cons <-
      case info of
        TyConI (DataD _ _ _ _ cons _) -> pure cons
        _ -> fail ("Failed to find data declaration for " ++ show nameStr)

    for cons \con ->
      case con of
        NormalC name []
          | Just str <- stripPrefix nameStr (nameBase name) ->
            pure (name, str)
        _ -> fail ("Unsupported constructor: " ++ show con)
