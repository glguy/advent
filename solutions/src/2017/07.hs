{-# Language QuasiQuotes, ImportQualifiedPost, DeriveTraversable #-}
{-|
Module      : Main
Description : Day 7 solution
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

<http://adventofcode.com/2017/day/7>

This task asks us to balance a nearly balanced tree. Each node
has its own weight and a balanced tree is one where each node's
children must all have the same total weight.

This solution uses a 'Node' type parameterized over the types of
the children so that it can be reused at multiple stages of the
computation.

Most solutions don't handle the case of a node with only
two children correctly, but fortunately for those solutions
AoC doesn't test solutions on this more challenging case.

-}
module Main
  ( -- $setup

    main

    -- * Types
  , Node(Node)
  , Summary(Summary)

    -- * Computation
  , topName
  , summarize
  , computeCorrection
  , corrections

    -- * Change tracking
  , OneChangeT(OCT)
  , change
  ) where

import Advent (format, pickOne, same)
import Advent.Fix (Fix, cataM, anaFromMap)
import Control.Applicative  (Alternative(empty,(<|>),some))
import Control.Monad (MonadPlus, ap, liftM)
import Data.Foldable (asum)
import Data.Functor.Classes (Show1(liftShowsPrec))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

-- $setup
--
-- Example tree from the problem description
--
-- @
--                gyxo
--               /
--          ugml - ebii
--        /      \\
--       |         jptl
--       |
--       |         pbga
--      \/        /
-- tknk --- padx - havc
--      \\        \\
--       |         qoyq
--       |
--       |         ktlj
--        \\      /
--          fwft - cntj
--               \\
--                 xhth
-- @
--
-- >>> :{
-- let example :: Map String (Node String)
--     example = Map.fromList
--       [ ("pbga", Node 66 [])
--       , ("xhth", Node 57 [])
--       , ("ebii", Node 61 [])
--       , ("havc", Node 66 [])
--       , ("ktlj", Node 57 [])
--       , ("fwft", Node 72 ["ktlj","cntj","xhth"])
--       , ("qoyq", Node 66 [])
--       , ("padx", Node 45 ["pbga","havc","qoyq"])
--       , ("tknk", Node 41 ["ugml","padx","fwft"])
--       , ("jptl", Node 61 [])
--       , ("ugml", Node 68 ["gyxo","ebii","jptl"])
--       , ("gyxo", Node 61 [])
--       , ("cntj", Node 57 [])]
-- :}
--
-- There are trickier tree problems when the unbalanced nodes have exactly
-- two children. In these cases you might need information from the parent
-- about which choice to make. See the documentation for 'summarize' to
-- see where the ambiguities arise and are resolved.
--
-- Note that locally node @b@ could be fixed by adjusting either of @d@
-- or @e@ but globally, only adjusting @d@ will work.
--
-- @
--             d (1)
--            /
--       b (4)
--      /     \\
-- a (1)       e (2)
--      \\
--       c (8)
-- @
--
-- >>> :{
-- let trickier :: Map String (Node String)
--     trickier = Map.fromList
--       [ ("a", Node 1 ["b","c"])
--       , ("b", Node 4 ["d","e"])
--       , ("c", Node 8 [])
--       , ("d", Node 1 [])
--       , ("e", Node 2 [])]
-- :}

-- | Representation of a node in the tree.
--
-- This type is parameterized so that we can either have the list of children
-- be a list of names of the children, or a list of actual child nodes, or
-- a list of weight summaries of the children.
data Node a = Node !Int [a] -- ^ Node weight and children
  deriving (Show, Functor, Foldable, Traversable)


-- | This instance is useful for showing the type @'Fix' 'Node'@
instance Show1 Node where
  liftShowsPrec _ s p (Node x y) =
    showParen (p >= 11)
      (showString "Node " . showsPrec 11 x . showChar ' ' . s y)


-- | Print the solutions to both parts of the task. The input file
-- can be overridden via command-line arguments.
main :: IO ()
main =
 do input <- [format|7 (%s %(%u%)(| -> %s&(, ))%n)*|]
    let nodes = Map.fromList [(n, Node c (fromMaybe [] xs)) | (n, c, xs) <- input]    
    -- part 1
    let top = topName nodes
    putStrLn top

    -- part 2
    print (computeCorrection (anaFromMap nodes top))


-- | Find the top-most name in the map of entries.
--
-- >>> topName example
-- "tknk"
-- >>> topName trickier
-- "a"
topName :: Ord name => Map name (Node name) -> name
topName m = Set.findMin
          $ Set.difference
              (Map.keysSet m)
              (Set.fromList (concatMap (\(Node _ xs) -> xs) m))


-- | Summary of a tree containing the root node's weight and the whole
-- tree's weight.
data Summary = Summary !Int !Int -- ^ top node weight, total weight
  deriving Show


-- | Given a tree, compute the 'Summary' for that tree and record
-- the new value of any node needed to balance the tree along the way.
--
-- This implementation uses a bottom-up fold of the tree. It computes
-- weight summaries of the tree while tracking the new value of any
-- nodes that needed to be changed to ensure that each node along the
-- way has equally weighted child trees.
--
-- >>> let summarizeExample = mapM_ print . runOneChangeT . summarize . anaFromMap example
--
-- >>> summarizeExample "ugml"
-- (Nothing,Summary 68 251)
--
-- >>> summarizeExample "padx"
-- (Nothing,Summary 45 243)
--
-- >>> summarizeExample "fwft"
-- (Nothing,Summary 72 243)
--
-- >>> summarizeExample "tknk"
-- (Just 60,Summary 41 770)
--
-- These next examples show how ambiguity can arise in a child node and then
-- be resolved in a parent.
--
-- >>> let summarizeTrickier = mapM_ print . runOneChangeT . summarize . anaFromMap trickier
--
-- >>> summarizeTrickier "b"
-- (Just 2,Summary 4 8)
-- (Just 1,Summary 4 6)
--
-- >>> summarizeTrickier "a"
-- (Just 2,Summary 1 17)
summarize :: Fix Node -> OneChangeT Int [] Summary
summarize = cataM $ \(Node n xs) ->

  if same [ w | Summary _ w <- xs ]

    then -- all children matched, no changes needed
      pure (Summary n (n + sum [ w | Summary _ w <- xs ]))

    else -- not all children matched, consider ways to fix this
      asum
         [ Summary n (n + length xs * newTree) <$ change newNode
         | Summary newNode newTree <- corrections xs ]

-- | Given a list of child node summaries, generate the possible corrected
-- child node weights and the resulting total child weight after that
-- change. It doesn't matter /which/ node is changed, so that isn't tracked.
--
-- With two children either might need to be fixed.
--
-- >>> corrections [Summary 3 6, Summary 4 8]
-- [Summary 5 8,Summary 2 6]
--
-- With more than two children it will be clear which is wrong.
--
-- >>> corrections [Summary 1 4, Summary 2 7, Summary 3 7 ]
-- [Summary 4 7]
--
-- If no corrections are needed, none are offered.
--
-- >>> corrections (replicate 2 (Summary 1 6))
-- []
-- >>> corrections [Summary 1 6]
-- []
-- >>> corrections []
-- []
corrections ::
  [Summary] {- ^ all child summaries            -} ->
  [Summary] {- ^ possible fixed child summaries -}
corrections xs =
  [ Summary (nodeWeight + discrepency) other

  | (Summary nodeWeight treeWeight, xs') <- pickOne xs
  , let weights = [ w | Summary _ w <- xs' ]
  , same weights            -- verify that all other children would now match
  , other <- take 1 weights -- all the element were same, consider one of them
  , let discrepency = other - treeWeight
  , discrepency /= 0
  ]


-- | Given a tree, compute the corrected weight to balance the whole tree.
-- If there are multiple possibilities this returns the one of them.
--
-- >>> computeCorrection (anaFromMap example "tknk")
-- Just 60
-- >>> computeCorrection (anaFromMap trickier "a")
-- Just 2
computeCorrection :: Fix Node -> Maybe Int
computeCorrection = (=<<) fst . listToMaybe . runOneChangeT . summarize


-- | A variant of the writer monad-transformer that "fails"
-- when more than one write is recorded.
newtype OneChangeT c m a = OCT { runOneChangeT :: m (Maybe c, a) }

-- | Record a change. Changes will collide even if they have the same value.
--
-- >>> runOneChangeT (pure ()) :: [(Maybe Bool, ())]
-- [(Nothing,())]
-- >>> runOneChangeT (change True) :: [(Maybe Bool, ())]
-- [(Just True,())]
-- >>> runOneChangeT (change True >> change True) :: [(Maybe Bool, ())]
-- []
-- >>> runOneChangeT (change True >> change False) :: [(Maybe Bool, ())]
-- []
change :: Monad m => c -> OneChangeT c m ()
change c = OCT (pure (Just c, ()))

-- | Inherit 'Functor' from 'Monad' implementation
instance MonadPlus m => Functor (OneChangeT c m) where
  fmap = liftM

-- | 'pure' returns the given value with no change recorded.
instance MonadPlus m => Applicative (OneChangeT c m) where
  pure x = OCT (pure (Nothing, x))
  (<*>)  = ap

-- | Inherit 'Alternative' from underlying type @m@
instance MonadPlus m => Alternative (OneChangeT c m) where
  OCT xs <|> OCT ys = OCT (xs <|> ys)
  empty             = OCT empty

-- | Inherit 'MonadPlus' from underlying type @m@
instance MonadPlus m => MonadPlus (OneChangeT c m)

-- | Sequencing of two values fails if both have recorded a change.
instance MonadPlus m => Monad (OneChangeT c m) where
  m >>= f = OCT $
    do (mb1,x) <- runOneChangeT m
       (mb2,y) <- runOneChangeT (f x)
       case (mb1,mb2) of
         (Nothing,_) -> pure (mb2,y)
         (_,Nothing) -> pure (mb1,y)
         _           -> empty


data Balance
  = Balanced (Set Int) -- one-hole contexts
  | Unbalanced (Map Int (Set Int))
               -- this new value can produce this new tree weight
  deriving Show

{-
summarize :: Fix Node -> Maybe (Int, Int, Balance)
summarize = cataM $ \(Node n xs) ->

  if same [ w | Summary _ w <- xs ]

    then -- all children matched, no changes needed
      pure (Summary n (n + sum [ w | Summary _ w <- xs ]))

    else -- not all children matched, consider ways to fix this
      asum
         [ Summary n (n + length xs * newTree) <$ change newNode
         | Summary newNode newTree <- corrections xs ]
-}
