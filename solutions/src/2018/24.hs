{-# Language RecordWildCards, TemplateHaskell, QuasiQuotes, ImportQualifiedPost #-}
{-|
Module      : Main
Description : Day 24 solution
Copyright   : (c) Eric Mertens, 2018
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2018/day/24>
-}
module Main (main) where

import Advent.Format (format)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List (foldl', maximumBy, delete, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

data Group = Group
  { size          :: !Int
  , hp            :: !Int
  , special       :: [(Element, Effect)]
  , attack        :: !Int
  , attackElement :: !Element
  , initiative    :: !Int
  , team          :: !Team
  }
  deriving (Show, Eq)

type Team = T
data T = Tgood | Tbad deriving (Eq, Ord, Show)

type Effect = E
data E = Eweak | Eimmune deriving (Eq, Ord, Show)

type Element = D
data D = Dbludgeoning | Dfire | Dslashing | Dradiation | Dcold
  deriving (Eq, Ord, Show)

mempty

-- | Print the answers to day 24
--
-- >>> :main
-- 16747
-- 5923
main :: IO ()
main =
 do (goods, bads) <- [format|2018 24
      Immune System:%n
      (%u units each with %u hit points(| %((@E to @D&(, ))&(; )%)) with an attack that does %u @D damage at initiative %u%n)*
      %n
      Infection:%n
      (%u units each with %u hit points(| %((@E to @D&(, ))&(; )%)) with an attack that does %u @D damage at initiative %u%n)*|]
    let toGroup team (size, hp, mbSpecial, attack, attackElement, initiative) =
          Group{ special = [(d, e) | (e, ds) <- fromMaybe [] mbSpecial, d <- ds], ..}
    let groups = map (toGroup Tgood) goods ++ map (toGroup Tbad) bads
    let groupMap = IntMap.fromList [ (initiative g, g) | g <- groups ]
    print (evaluate (simulate groupMap))
    print (evaluate (search (attemptBoost groupMap) 1 Nothing))

-- | Find the sum of the sizes of all the groups.
evaluate :: IntMap Group -> Int
evaluate = sum . fmap size

-- | Determine the lowest value that satisfies the given
-- predicate and return the result.
search ::
  (Int -> Maybe a) {- ^ satisfication condition -} ->
  Int              {- ^ known too low bound     -} ->
  Maybe (Int, a)   {- ^ known satisfying int    -} ->
  a                {- satisfying result         -}
search f tooLo (Just (hi, best)) | tooLo + 1 == hi = best
search f tooLo mbHi =
  case f i of
    Nothing   -> search f i mbHi
    Just best -> search f tooLo (Just (i, best))
  where
    i = case mbHi of
          Nothing     -> tooLo*2
          Just (hi,_) -> (tooLo+1 + hi) `quot` 2

-- | Test if a group is on the reindeer team.
isGood :: Group -> Bool
isGood g = Tgood == team g

-- | Determine if a boost is enough to allow the reindeer to
-- win. If it is return the final group of reindeer.
attemptBoost :: IntMap Group -> Int -> Maybe (IntMap Group)
attemptBoost groups boost
  | all isGood outcome = Just outcome
  | otherwise          = Nothing
  where
    boostGood g
      | isGood g  = g { attack = attack g + boost }
      | otherwise = g

    outcome = simulate (fmap boostGood groups)

-- battle logic --------------------------------------------------------

-- | Run a battle until it stops making progress due to immunities
-- or due to a team being wiped out.
simulate :: IntMap Group -> IntMap Group
simulate groups
  | groups == groups' = groups
  | otherwise         = simulate groups'
  where
    groups' = combat groups

-- | Determine the effectiveness multiplier of an attack element against
-- a particular group.
effectiveness ::
  Group {- ^ attacker          -} ->
  Group {- ^ defender          -} ->
  Int   {- ^ damage multiplier -}
effectiveness atk def =
  case lookup (attackElement atk) (special def) of
    Just Eimmune -> 0
    Nothing      -> 1
    Just Eweak   -> 2

-- | Compute effective power of a group
effectivePower :: Group -> Int
effectivePower grp = size grp * attack grp

-- | Order a list of groups by the order they get to chose their
-- targets.
targetSelectionOrder ::
  [Group] {- ^ unordered groups        -} ->
  [Group] {- ^ target preference order -}
targetSelectionOrder = sortOn prj
  where
    -- ascending sort lexicographic order:
    -- * effectiveness (negated for descending order)
    -- * initiative    (negated for descending order)
    prj grp = (negate (effectivePower grp), negate (initiative grp))

-- | Given a list of groups generate a targetting assignment.
targetSelection ::
  [Group]    {- ^ unordered groups        -} ->
  IntMap Int {- ^ attacker / defender IDs -}
targetSelection groups =
  fst (foldl' chooseTarget (IntMap.empty, groups) (targetSelectionOrder groups))

-- | Given the current targetting assignment and a list of the groups
-- not yet targeted, update the assignment and list given the
-- preferences of the next attacker.
chooseTarget ::
  (IntMap Int, [Group]) {- ^ targets so far and remaining groups -} ->
  Group                 {- ^ attacker                            -} ->
  (IntMap Int, [Group])
chooseTarget (targets, groups) atk =
  fromMaybe (targets, groups) $ -- use previous values if no target
  do def <- targetChoice atk groups
     let targets' = IntMap.insert (initiative atk) (initiative def) targets
         groups'  = delete def groups
     targets' `seq` Just (targets', groups')

-- | Given a group and the list of remaining target choices, determine
-- the chosen group, if any.
targetChoice ::
  Group   {- ^ attacker          -} ->
  [Group] {- ^ elligible targets -} ->
  Maybe Group
targetChoice atk groups = maximumOn prj (filter (isValidTarget atk) groups)
  where
    -- maximum lexicographic order:
    -- * effectiveness   (prefer targeting groups weak to this unit)
    -- * effective power (prefer targeting strong groups)
    -- * initiative      (prefer targeting high initiative groups)
    prj def = (effectiveness atk def, effectivePower def, initiative def)

-- | Check if one unit can attack another.
isValidTarget ::
  Group {- ^ attacker            -} ->
  Group {- ^ defender            -} ->
  Bool  {- ^ valid attack choice -}
isValidTarget atk def = team atk /= team def      -- no friendly fire
                     && effectiveness atk def > 0 -- skip if immune

-- | Given an unordered list of groups, compute the result of combat
combat :: IntMap Group -> IntMap Group
combat groups = foldl' (combatTurn targets) groups combatOrder
  where
    targets     = targetSelection (IntMap.elems groups)

    combatOrder = reverse (IntMap.keys groups)

-- Apply the combat effect for a specific group identified by its ID
combatTurn ::
  IntMap Int   {- ^ targets        -} ->
  IntMap Group {- ^ all groups     -} ->
  Int          {- ^ attacker ID    -} ->
  IntMap Group {- ^ updated groups -}
combatTurn targets groups atkid =
  fromMaybe groups $ -- in case of no target or dead group, no change
  do defid <- IntMap.lookup atkid targets
     atk   <- IntMap.lookup atkid groups
     def   <- IntMap.lookup defid groups
     let dmg    = effectiveness atk def * effectivePower atk
         killed = min (size def) (dmg `quot` hp def)
         size'  = size def - killed
         groups'
           | size' > 0 = IntMap.insert defid def { size = size' } groups
           | otherwise = IntMap.delete defid groups
     Just groups'

-- foldable utility function -------------------------------------------

-- | Returns the element that maximizes the result of a key function
-- applied to the elements. See also: 'sortOn'
maximumOn :: (Foldable t, Ord b) => (a -> b) -> t a -> Maybe a
maximumOn prj xs
  | null xs   = Nothing
  | otherwise = Just $! maximumBy (comparing prj) xs

