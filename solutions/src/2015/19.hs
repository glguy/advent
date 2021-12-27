module Main (main) where

import Data.Array
import Data.Char
import Data.Map ( Map )
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

newtype Atom = Atom String
  deriving (Eq,Ord)

main :: IO ()
main =
  do (rules, input) <- loadInput
     print (minRulesNeeded rules input (Atom "e"))

loadInput :: IO ( Map Atom [[Atom]], [Atom] )
loadInput =
  do xs <- lines <$> readFile "input19.txt"
     return $! case break null xs of
       (rules, ["", initial]) -> (toMap (parseRule <$> rules), parseMolecule initial)
       _ -> error "Bad input"

toMap :: Ord k => [(k,v)] -> Map k [v]
toMap = Map.fromListWith (++) . map (fmap pure)

-- | Add empty elements to the map so that every @a@ that occurs in
-- the values of the map also occurs in the keys.
extendRules :: Ord a => Map a [[a]] -> Map a [[a]]
extendRules rules = Map.unionWith (++) extraRules rules
  where
  extraRules = Map.fromSet (const [])
             $ Set.fromList (concat (concat (Map.elems rules)))

-- |
-- > parseRule "A => BC"
-- (Atom "A", [Atom "B", Atom "C"])
parseRule :: String -> (Atom, [Atom])
parseRule str =
  case words str of
    [x,"=>",y] -> (Atom x,parseMolecule y)
    _ -> error ("Bad line: " ++ str)

-- |
-- > parseMolecule "AbCdEF"
-- [Atom "Ab", Atom "Cd", Atom "E", Atom "F"]
parseMolecule :: String -> [Atom]
parseMolecule str =
  case str of
    ""  -> []
    x:xs -> case break isUpper xs of
              (y,ys) -> Atom (x:y) : parseMolecule ys

-- | Given a map of rewrite rules rewriting the keys to any of the
-- alternatives, return the minimum number of rewrites needed to rewrite
-- the start symbol into the input.
minRulesNeeded ::
  Ord a =>
  Map a [[a]] {- ^ rules, sum of products -} ->
  [a]         {- ^ input                  -} ->
  a           {- ^ start state            -} ->
  Maybe Int
minRulesNeeded rules input start = minRulesNeededInt ruleArr inputArr (toInt start)
  where
  rules'  = extendRules rules
  toInt x = Map.findIndex x rules'

  numRules = Map.size rules'
  numInput = length input
  inputArr = listArray (0,numInput-1) (map toInt input)
  ruleArr  = listArray (0,numRules-1) (Map.elems (fmap (map (map toInt)) rules'))

-- | Given an array of inputs determine how many rule applications
-- are required to transform the start state into the input.
--
-- This solution uses dynamic programming. The solutions are memoized
-- as about how many steps, if any, each substring of the input takes to
-- match each of the symbols in the alphabet.
minRulesNeededInt ::
  Ix i =>
  Array i [[i]] {- ^ rules, sum of products -} ->
  Array Int i   {- ^ input -} ->
  i             {- ^ start -} ->
  Maybe Int     {- ^ minimum rules needed -}
minRulesNeededInt rules input = cost inputLo inputHi
  where
  (inputLo,inputHi) = bounds input
  (rulesLo,rulesHi) = bounds rules
  costBounds        = ((inputLo,inputLo,rulesLo)
                      ,(inputHi,inputHi,rulesHi))

  cost start end rule = costArray ! (start,end,rule)

  costArray = generate costBounds cost'

  cost' (start,end,ruleIx)
    | start == end, input ! start == ruleIx = Just 0
    | otherwise = fmap succ
                $ minimum'
                $ mapMaybe (nonTerm start end)
                $ rules ! ruleIx

  nonTerm start end rhs =
    case rhs of
     []   -> Nothing
     [x]  -> cost start end x
     x:xs -> minimum'
               [ cost1 + cost2
               | mid   <- [start .. end - length xs]
               , cost1 <- maybeToList (cost start mid x)
               , cost2 <- maybeToList (nonTerm (succ mid) end xs)
               ]

-- | Generate an array given the bounds an a function from indexes to elements.
generate :: Ix i => (i,i) -> (i -> e) -> Array i e
generate bnd f = array bnd [ (i, f i) | i <- range bnd ]
         
-- | Returns the minimum element of a list unless the list is empty.
minimum' :: Ord a => [a] -> Maybe a
minimum' [] = Nothing
minimum' xs = Just $! minimum xs
