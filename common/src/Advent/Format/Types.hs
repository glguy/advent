module Advent.Format.Types where

data Token
  = TOpenGroup
  | TCloseGroup
  | TAnyChar
  | TAnyLetter
  | TAnyWord
  | TUnsignedInteger
  | TSignedInteger
  | TUnsignedInt
  | TSignedInt
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
  -- primitives
  | Literal String
  | UnsignedInteger
  | SignedInteger
  | UnsignedInt
  | SignedInt
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
    Alt x y             -> interesting x || interesting y
    Follow x y          -> interesting x || interesting y
    Empty               -> False
    UnsignedInteger     -> True
    SignedInteger       -> True
    UnsignedInt         -> True
    SignedInt           -> True
    Word                -> True
    Char                -> True
    Letter              -> True
    Gather{}            -> True
    Named{}             -> True
    Literal{}           -> False

acceptsEmpty :: Format -> Bool
acceptsEmpty fmt =
  case fmt of
    Many _              -> True
    Some x              -> acceptsEmpty x
    SepBy _ _           -> True
    Alt x y             -> acceptsEmpty x || acceptsEmpty y
    Follow x y          -> acceptsEmpty x && acceptsEmpty y
    Empty               -> True
    UnsignedInteger     -> False
    SignedInteger       -> False
    UnsignedInt         -> False
    SignedInt           -> False
    Word                -> False
    Char                -> False
    Letter              -> False
    Gather x            -> acceptsEmpty x
    Named{}             -> False
    Literal x           -> null x

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
    UnsignedInteger     -> showString "%lu"
    SignedInteger       -> showString "%ld"
    UnsignedInt         -> showString "%u"
    SignedInt           -> showString "%d"
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
    TUnsignedInt      -> "%u"
    TSignedInt        -> "%d"
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
