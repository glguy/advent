{-# Language BlockArguments, TemplateHaskell, ViewPatterns #-}
{-|
Module      : Advent.Format
Description : Input file format quasiquoter
Copyright   : (c) Eric Mertens, 2018-2021
License     : ISC
Maintainer  : emertens@gmail.com

Usage: @[format|<day> <format string>|]@

When day is specified as @0@ the quasiquoter returns a pure
parser function. Otherwise day uses command line arguments
to find the input file and parses it as an IO action.

A format string can optionally be specified across multiple
lines. In this case the day number goes on the first line and
the pattern starts on the second line. All common leading white
space from all the remaining lines is trimmed off and newlines
are discarded (use @%n@ for matching newlines)

The following are identical:

@
example1 = [format|2021 1
    %s%n
    %s%n|]

example2 = [format|2021 1 %s%n%s%n|]
@

Patterns:

    * @%u@ unsigned integer as 'Int'
    * @%d@ signed integer as 'Int'
    * @%x@ unsigned hexadecimal as 'Int'
    * @%lu@ unsigned integer as 'Integer'
    * @%ld@ signed integer as 'Integer'
    * @%lx@ unsigned hexadecimal as 'Integer'
    * @%s@ non-empty list of non-space characters as 'String'
    * @%c@ single, non-newline character as 'Char'
    * @%a@ single ASCII letter as 'Char'
    * @%n@ single newline character
    * other characters match literally
    * use @%@ to escape literal matches of special characters
    * @\@A@ matches the names of the constructors of type @A@ as an @A@

Structures:

    * @p|q@ combine alternatives @p@ and @q@
    * @(pq)@ group subpattern @pq@
    * @p*@ zero-to-many repititions of @p@ as a '[]'
    * @p+@ one-to-many repititions of @p@ as a '[]'
    * @p&q@ zero-to-many repititions of @p@ separated by @q@ as a '[]'
    * @p!@ returns the characters that matched pattern @p@ as a 'String'

-}
module Advent.Format (format) where

import Advent.Format.Enum (enumCases)
import Advent.Format.Lexer (alexScanTokens, AlexPosn(..))
import Advent.Format.Parser (parseFormat, ParseError(..))
import Advent.Format.Show (showFormat, showToken)
import Advent.Format.Types (Format(..))
import Advent.Format.Utils
import Advent.Input (getRawInput)
import Advent.Prelude (countBy)
import Control.Applicative ((<|>), some)
import Control.Monad ((<=<), when, void)
import Data.Char (isDigit, isSpace, isUpper, isAsciiLower, isAsciiUpper, isHexDigit)
import Data.Maybe (listToMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Numeric (readHex)
import Text.ParserCombinators.ReadP

parse :: String -> Q Format
parse txt =
  case parseFormat (alexScanTokens txt) of
    Right fmt                -> pure (simplify fmt)
    Left (Unclosed p)        -> failAt p "Unclosed parenthesis"
    Left (UnexpectedToken p t) -> failAt p ("Unexpected token " ++ showToken t)
    Left UnexpectedEOF       -> fail "Format parse error, unexpected end-of-input"

failAt :: AlexPosn -> String -> Q a
failAt (AlexPn _ line col) msg = fail ("Format parse error at " ++ show line ++ ":" ++ show col ++ ", " ++ msg)

-- | Constructs an input parser. See "Advent.Format"
format :: QuasiQuoter
format = QuasiQuoter
  { quoteExp  = uncurry makeParser <=< prepare
  , quotePat  = \_ -> fail "format: patterns not supported"
  , quoteType = toType <=< parse . snd <=< prepare
  , quoteDec  = makeDecs
  }

prepare :: String -> Q (Maybe (Int, Int), String)
prepare str =
  -- Git on Windows has a bad behavior where it can add \r to files
  case lines (filter ('\r' /=) str) of
    []   -> fail "Empty input format"
    [x]
      | Just (yd, str') <- splitLeader x -> pure (yd, str')
      | otherwise -> fail "Failed to parse single-line input pattern"
    x:xs
      | Just (yd, "") <- splitLeader x ->
        pure (yd, concatMap (drop indent) xs1)
      where
        xs1    = filter (any (' ' /=)) xs
        indent = minimum (map (length . takeWhile (' '==)) xs1)
    _ -> fail "Failed to parse multi-line input pattern"

splitLeader :: String -> Maybe (Maybe (Int, Int), String)
splitLeader (reads -> [(y,reads -> [(d, rest)])]) = Just (Just (y, d), dropWhile (' '==) rest)
splitLeader (lex   -> [("-", rest)]) = Just (Nothing, dropWhile (' '==) rest)
splitLeader _ = Nothing

makeDecs :: String -> DecsQ
makeDecs str =
 do fmt <- parse str
    [d|
      type Input = $(toType fmt)

      parseInput :: String -> Input
      parseInput str =
        case readP_to_S ($(toReadP fmt) <* eof) str of
          (x, _) : _ -> x
          _          -> error "bad input parse"

      getInput :: Int -> Int -> IO Input
      getInput y d = parseInput <$> getRawInput y d
      |]

makeParser :: Maybe (Int, Int) -> String -> ExpQ
makeParser mb str =
  do fmt <- parse str
     let formats = [| readP_to_S ($(toReadP fmt) <* eof) |]
     let qf = [| maybe (error "bad input parse") fst . listToMaybe . $formats |]
     case mb of
       Nothing    -> qf
       Just (y,d) -> [| $qf <$> getRawInput y d |]

toReadP :: Format -> ExpQ
toReadP s =
  case s of
    Literal xs -> [| void (string xs) |]

    Gather p
      | interesting p -> [|         gather $(toReadP p) |]
      | otherwise     -> [| fst <$> gather $(toReadP p) |]

    Named n
      | isUpper (head n) -> enumParser n
      | otherwise -> varE (mkName n)

    UnsignedInteger -> [| (read :: String -> Integer) <$>                                      munch1 isDigit  |]
    SignedInteger   -> [| (read :: String -> Integer) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]
    UnsignedInt     -> [| (read :: String -> Int    ) <$>                                      munch1 isDigit  |]
    SignedInt       -> [| (read :: String -> Int    ) <$> ((++) <$> option "" (string "-") <*> munch1 isDigit) |]

    HexInteger      -> [| (fst . head . readHex :: String -> Integer) <$> munch1 isHexDigit |]
    HexInt          -> [| (fst . head . readHex :: String -> Int    ) <$> munch1 isHexDigit |]

    Char      -> [| satisfy ('\n' /=) |]
    Letter    -> [| satisfy (\x -> isAsciiLower x || isAsciiUpper x) |]
    Word      -> [| some (satisfy (not . isSpace)) |]

    Many x ->
     do whenM (acceptsEmpty x) (fail ("Argument to * accepts ε: " ++ showFormat 0 s ""))
        if interesting x then
          [| many $(toReadP x) |]
        else
          [| void (many $(toReadP x)) |]

    Some x ->
     do whenM (acceptsEmpty x) (fail ("Argument to + accepts ε: " ++ showFormat 0 s ""))
        if interesting x then
          [| some $(toReadP x) |]
        else
          [| void (some $(toReadP x)) |]

    SepBy x y ->
     do whenM (allM acceptsEmpty [x, y]) (fail ("Both arguments to & accept ε: " ++ showFormat 0 s ""))
        if interesting x then
          [| sepBy $(toReadP x) $(toReadP y) |]
        else
          [| void (sepBy $(toReadP x) $(toReadP y)) |]

    Alt x y
      | xi, yi    -> [| Left    <$> $xp <|> Right   <$> $yp |]
      | xi        -> [| Just    <$> $xp <|> Nothing <$  $yp |]
      |     yi    -> [| Nothing <$  $xp <|> Just    <$> $yp |]
      | otherwise -> [|             $xp <|>             $yp |]
      where
        xi = interesting x
        yi = interesting y
        xp = toReadP x
        yp = toReadP y

    Group x -> toReadP x

    Follow xs ->
      case [(interesting x, toReadP x) | x <- xs] of
        [] -> [| pure () |]
        xxs@((ix,x):xs)
          | n == 0    -> foldl apply0 x xs
          | n <= 1    -> foldl apply1 x xs
          | ix        -> foldl applyN [| $tup <$> $x |] xs
          | otherwise -> foldl applyN [| $tup <$  $x |] xs
          where
            tup            = conE (tupleDataName n)
            n              = countBy fst xxs
            apply0 l (_,r) = [| $l *> $r |]
            apply1 l (i,r) = if i then [| $l  *> $r |] else [| $l <* $r |]
            applyN l (i,r) = if i then [| $l <*> $r |] else [| $l <* $r |]

toType :: Format -> TypeQ
toType fmt =
  case fmt of
    Literal _ -> [t| () |]

    Gather x
      | interesting x -> [t| (String, $(toType x)) |]
      | otherwise     -> [t| String |]

    Named n
      | isUpper (head n) -> conT (mkName n)
      | otherwise -> fail "toType: not implemented for variable yet"

    UnsignedInteger -> [t| Integer |]
    SignedInteger   -> [t| Integer |]
    HexInteger      -> [t| Integer |]
    UnsignedInt     -> [t| Int |]
    SignedInt       -> [t| Int |]
    HexInt          -> [t| Int |]

    Char      -> [t| Char |]
    Letter    -> [t| Char |]
    Word      -> [t| String |]

    Many x ->
     do whenM (acceptsEmpty x) (fail ("Argument to * accepts ε: " ++ showFormat 0 fmt ""))
        if interesting x then
          [t| [$(toType x)] |]
        else
          [t| () |]

    Some x ->
     do whenM (acceptsEmpty x) (fail ("Argument to + accepts ε: " ++ showFormat 0 fmt ""))
        if interesting x then
          [t| [$(toType x)] |]
        else
          [t| () |]

    SepBy x y ->
     do whenM (allM acceptsEmpty [x, y]) (fail ("Both arguments to & accept ε: " ++ showFormat 0 fmt ""))
        if interesting x then
          [t| [$(toType x)] |]
        else
          [t| () |]

    Alt x y
      | xi, yi    -> [t| Either $xt $yt |]
      | xi        -> [t| Maybe $xt |]
      |     yi    -> [t| Maybe $yt |]
      | otherwise -> [t| () |]
      where
        xi = interesting x
        yi = interesting y
        xt = toType x
        yt = toType y

    Group x -> toType x

    Follow xs ->
      case [toType x | x <- xs, interesting x] of
        [] -> [t| () |]
        [t] -> t
        ts -> foldl appT (tupleT (length ts)) ts

enumParser :: String -> ExpQ
enumParser nameStr =
  do entries <- enumCases nameStr
     let parsers = [[| $(conE name) <$ string str |] | (name, str) <- entries]
     [| choice $(listE parsers) |]

whenM :: Monad m => m Bool -> m () -> m ()
whenM pm m = pm >>= \p -> when p m
