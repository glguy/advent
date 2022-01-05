{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 22 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/22>

-}
module Main where

import Advent.Format (format)
import Advent.Search (astar, AStep(..))

main :: IO ()
main =
 do (hp,dmg) <- [format|2015 22 Hit Points: %u%nDamage: %u%n|]
    print (runSim (initialState hp dmg))
    print (runSim (initialState hp dmg) { difficulty = 1 })

runSim :: GameState -> Int
runSim s = head [cost | (s1, cost) <- astar advance s, bossDead s1]

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
  , poisonTimer  
  , rechargeTimer
  , shieldTimer  
  , playerHp     
  , bossHp, bossDamage, difficulty :: !Int
  }
  deriving (Eq, Ord, Show)

initialState :: Int -> Int -> GameState
initialState hp dmg = GameState
  { manaPool      = 500
  , poisonTimer   = 0
  , rechargeTimer = 0
  , shieldTimer   = 0
  , playerHp      = 50
  , bossHp        = hp
  , bossDamage    = dmg
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

advance :: GameState -> [AStep GameState]
advance s =
  [ AStep s5 (spellCost spell) 0
  | s1 <- s --> stepTimers . applyDifficulty
  , spell <- availableSpells s1
  , s2 <- s1 --> applySpell spell
  , s3 <- s2 --> stepTimers
  , s4 <- s3 --> bossAttack
  , s5 <- s4 --> id
  ]

infix 1 -->

(-->) :: GameState -> (GameState -> GameState) -> [GameState]
s --> k
  | playerDead s = []
  | bossDead s   = [s]
  | otherwise    = [k s]

applySpell :: Spell -> GameState -> GameState
applySpell spell s =
  s { manaPool      = manaPool s - spellCost spell
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

bossDead :: GameState -> Bool
bossDead s = bossHp s <= 0

playerDead :: GameState -> Bool
playerDead s = playerHp s <= 0
