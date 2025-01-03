{-# Language BlockArguments #-}
module Advent.Format.Enum where

import Language.Haskell.TH
import Data.List
import Data.Traversable

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
            case str of
              '_' : symbolName ->
                do symbol <- processSymbolName symbolName
                   pure (name, symbol)
              _ -> pure (name, str)
        _ -> fail ("Unsupported constructor: " ++ show con)

processSymbolName :: String -> Q String
processSymbolName str =
  case break ('_' ==) str of
    (name, rest) ->
      case lookup name symbolNames of
        Nothing -> fail ("Unknown symbol name: " ++ name)
        Just symbol ->
          case rest of
            [] -> pure [symbol]
            _:str' -> (symbol:) <$> processSymbolName str'

symbolNames :: [(String, Char)]
symbolNames =
  [ ("LT", '<')
  , ("GT", '>')
  , ("EQ", '=')
  , ("BANG", '!')
  , ("AT" , '@')
  , ("HASH", '#')
  , ("DOLLAR", '$')
  , ("PERCENT", '%')
  , ("CARET", '^')
  , ("AMPERSAND", '&')
  , ("STAR", '*')
  , ("PIPE", '|')
  , ("LPAREN", '(')
  , ("RPAREN", ')')
  , ("LBRACE", '{')
  , ("RBRACE", '}')
  , ("LBRACK", '[')
  , ("RBRACK", ']')
  , ("COLON", ':')
  , ("SEMI", ';')
  , ("QUESTION", '?')
  , ("SLASH", '/')
  , ("BACKSLASH", '\\')
  , ("UNDERSCORE", '_')
  , ("DASH", '-')
  , ("DOT", '.')
  , ("COMMA", ',')
  , ("PLUS", '+')
  , ("TILDE", '~')
  , ("SPACE", ' ')
  ]
