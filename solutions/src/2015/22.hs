module Main where

import Control.Monad
import Data.List
import qualified Data.IntMap as IntMap
import Data.IntMap ( IntMap )

main :: IO ()
main =
  do print (runSim initialState)
     print (runSim initialState { difficulty = 1 })

runSim :: GameState -> Int
runSim s = search (IntMap.singleton 0 [s]) maxBound

data Spell = Recharge | Poison | Shield | Drain | MagicMissile
  deriving Eq

spellDamage :: Spell -> Int
spellDamage spell =
  case spell of
    MagicMissile -> 4
    Drain        -> 2
    _            -> 0

spellHeal :: Spell -> Int
spellHeal spell =
  case spell of
    Drain        -> 2
    _            -> 0

spellCost :: Spell -> Int
spellCost s =
  case s of
    Recharge -> 229
    Poison   -> 173
    Shield   -> 113
    Drain    -> 73
    MagicMissile -> 53


data GameState = GameState
  { manaPool     
  , manaSpent
  , poisonTimer  
  , rechargeTimer
  , shieldTimer  
  , playerHp     
  , bossHp, bossDamage, difficulty :: !Int
  }
  deriving (Eq, Show)

initialState :: GameState
initialState = GameState
  { manaPool      = 500
  , manaSpent     = 0
  , poisonTimer   = 0
  , rechargeTimer = 0
  , shieldTimer   = 0
  , playerHp      = 50
  , bossHp        = 58
  , bossDamage    = 9
  , difficulty    = 0
  }

stepTimers :: GameState -> GameState
stepTimers s = s
  { manaPool      = manaPool s +
                    if rechargeTimer s > 0 then 101 else 0
  , bossHp        = bossHp s -
                    if poisonTimer s > 0 then 3 else 0
  , poisonTimer   = dec (poisonTimer s)
  , rechargeTimer = dec (rechargeTimer s)
  , shieldTimer   = dec (shieldTimer s)
  }

bossAttack :: GameState -> GameState
bossAttack s = s { playerHp = playerHp s - effectiveAttack }
  where
  effectiveAttack = max 1 (bossDamage s - armor)
  armor | shieldTimer s > 0 = 7
        | otherwise         = 0

applyDifficulty :: GameState -> GameState
applyDifficulty s = s { playerHp = playerHp s - difficulty s }

advance :: GameState -> [GameState]
advance s =
  stepTimers (applyDifficulty s)   --> \s1 ->
  availableSpells s1               >>=  \spell ->
  stepTimers (applySpell spell s1) --> \s2 ->
  bossAttack s2                    --> \s3 ->
  return s3

infixl 1 -->

(-->) :: GameState -> (GameState -> [GameState]) -> [GameState]
s --> k
  | playerDead s = []
  | bossDead s   = [s]
  | otherwise    = k s

applySpell :: Spell -> GameState -> GameState
applySpell spell s =
  s { manaSpent     = manaSpent s + spellCost spell
    , manaPool      = manaPool s - spellCost spell
    , rechargeTimer = if spell == Recharge then 5 else rechargeTimer s
    , poisonTimer   = if spell == Poison   then 6 else poisonTimer s
    , shieldTimer   = if spell == Shield   then 6 else shieldTimer s
    , bossHp        = bossHp s - spellDamage spell
    , playerHp      = playerHp s + spellHeal spell
    }

availableSpells :: GameState -> [Spell]
availableSpells s =
    filter (\spell -> spellCost spell <= manaPool s)
    $ [Poison   | poisonTimer s == 0]
   ++ [Recharge | rechargeTimer s == 0]
   ++ [Shield   | shieldTimer s == 0]
   ++ [MagicMissile, Drain]

dec :: Int -> Int
dec x | x <= 1 = 0
dec x          = x-1

-- Search the frontier of possible game states for the minimum possible mana
-- needed to kill a boss. The frontier is advanced by stepping one full turn
-- for each state picking the states with the minimum mana used so far.
-- Once the best seen so far is as good as the best states in the frontier
-- we know we're done because mana spending is monotonic.
search ::
  IntMap [GameState] {- ^ search frontier indexed by mana spent  -} ->
  Int                {- ^ lowest mana used to kill boss so far   -} ->
  Int                {- ^ lowest possible mana used to kill boss -}
search states best =
  case IntMap.minViewWithKey states of
    Nothing -> best
    Just ((k,ss),states')
      | best <= k -> best
      | otherwise -> search (foldl' schedule states' nextss) best'
      where
      nextss  = nub (concatMap advance ss)
      best'   = minimum (best : map manaSpent (filter bossDead nextss))

      schedule m t = IntMap.insertWith (++) (manaSpent t) [t] m

bossDead :: GameState -> Bool
bossDead s = bossHp s <= 0

playerDead :: GameState -> Bool
playerDead s = playerHp s <= 0
