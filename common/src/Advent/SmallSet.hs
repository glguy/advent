{-# Language TypeFamilies, TypeOperators, BlockArguments #-}
{-|
Module      : Advent.SmallSet
Description : An efficient set representation for small integers.
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

A compact set type for when you have very few elements to track.

-}
module Advent.SmallSet where

import Data.Bits
import Data.Coerce (coerce)
import Data.List (foldl')
import Data.MemoTrie (HasTrie(..))
import Data.Word (Word64)

-- | Sets of integers from 0 to 63 efficiently implemented using a Word64
newtype SmallSet = SmallSet Word64
  deriving (Eq, Ord)

-- | Construct a set given a list of set members.
--
-- >>> fromList []
-- fromList []
--
-- >>> fromList [0]
-- fromList [0]
--
-- >>> fromList [63]
-- fromList [63]
--
-- >>> fromList [0,10,20]
-- fromList [0,10,20]
fromList :: [Int] -> SmallSet
fromList = foldl' (flip insert) empty

-- | Return an ordered list of the elements in the set
toList :: SmallSet -> [Int]
toList (SmallSet s) = go 0 s
  where
    go offset n
      | 0 == n     = []
      | next == 63 = [63] -- avoid shift overflow
      | otherwise  = x : go (1+x) (n `unsafeShiftR` (next+1))
      where
        next = countTrailingZeros n
        x    = offset + next

-- | Predicate for integer elements that fit in a 'SmallSet'
inRange :: Int -> Bool
inRange x = 0 <= x && x < 64

-- | The set with no members.
--
-- >>> empty
-- fromList []
empty :: SmallSet
empty = SmallSet 0

-- | Predicate for empty sets.
null :: SmallSet -> Bool
null (SmallSet x) = 0 == x

-- | Make a new set with a single element
--
-- >>> singleton 42
-- fromList [42]
singleton :: Int -> SmallSet
singleton x
  | inRange x = SmallSet (bit x)
  | otherwise = error ("Advent.SmallSet.singleton: bad argument " ++ show x)

-- | Compute the union of two sets
--
-- >>> union (fromList [3,4,5,6]) (fromList [5,6,7,8])
-- fromList [3,4,5,6,7,8]
union :: SmallSet -> SmallSet -> SmallSet
union (SmallSet x) (SmallSet y) = SmallSet (x .|. y)

-- | Union of the sets in a list
--
-- >>> unions []
-- fromList []
--
-- >>> unions [singleton 1, fromList [2,4], fromList [2,3]]
-- fromList [1,2,3,4]
unions :: [SmallSet] -> SmallSet
unions = foldl' union empty

-- | Compute the intersection of two sets
--
-- >>> intersection (fromList [3,4,5,6]) (fromList [5,6,7,8])
-- fromList [5,6]
intersection :: SmallSet -> SmallSet -> SmallSet
intersection (SmallSet x) (SmallSet y) = SmallSet (x .&. y)

-- | Subtract the elements of the second set from the first set.
--
-- >>> difference (fromList [3,4,5,6]) (fromList [5,6,7,8])
-- fromList [3,4]
difference :: SmallSet -> SmallSet -> SmallSet
difference (SmallSet x) (SmallSet y) = SmallSet (x .&. complement y)

-- | Operator for 'difference'
(\\) :: SmallSet -> SmallSet -> SmallSet
(\\) = difference

infix 5 \\

-- | Add an element to a set
--
-- >>> insert 10 (fromList [3,4,5])
-- fromList [3,4,5,10]
--
-- >>> insert 5 (fromList [3,4,5])
-- fromList [3,4,5]
insert :: Int -> SmallSet -> SmallSet
insert x (SmallSet y)
  | inRange x = SmallSet (setBit y x)
  | otherwise = error ("Advent.SmallSet.insert: bad argument " ++ show x)

-- | Remove an element from a set
--
-- >>> delete 5 (fromList [3,4,5])
-- fromList [3,4]
--
-- >>> delete 8 (fromList [3,4,5])
-- fromList [3,4,5]
delete :: Int -> SmallSet -> SmallSet
delete x (SmallSet y)
  | inRange x = SmallSet (clearBit y x)
  | otherwise = error ("Advent.SmallSet.insert: bad argument " ++ show x)

-- | Check if a set contains an element
--
-- >>> member 8 (fromList [3,4,5])
-- False
--
-- >>> member 4 (fromList [3,4,5])
-- True
member :: Int -> SmallSet -> Bool
member x (SmallSet y)
  | inRange x = testBit y x
  | otherwise = error ("Advent.SmallSet.member: bad argument " ++ show x)

-- | Number of elements in a set.
--
-- >>> size (fromList [1,2,3])
-- 3
--
-- >>> size empty
-- 0
size :: SmallSet -> Int
size (SmallSet x) = popCount x

setRep :: SmallSet -> Word64
setRep (SmallSet x) = x

-- | Shows a 'SmallSet' using 'fromList' syntax
instance Show SmallSet where
  showsPrec p x = showParen (p > 10) (showString "fromList " . shows (toList x))

-- | Reads a 'SmallSet' using 'fromList' syntax
instance Read SmallSet where
  readsPrec p = readParen (p > 10) \s ->
    [(fromList xs, s2) | ("fromList", s1) <- lex s, (xs, s2) <- reads s1]

-- | Instance derived from: 'HasTrie' 'Word64'
instance HasTrie SmallSet where
  newtype SmallSet :->: a = T (Word64 :->: a)
  trie = (coerce :: ((Word64 -> a) -> Word64 :->: a) -> (SmallSet -> a) -> SmallSet :->: a) trie
  untrie = (coerce :: (Word64 :->: a -> Word64 -> a) -> SmallSet :->: a -> SmallSet -> a) untrie
  enumerate = (coerce :: (Word64 :->: a -> [(Word64, a)]) -> SmallSet :->: a -> [(SmallSet, a)]) enumerate
