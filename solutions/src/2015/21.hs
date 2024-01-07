{-# Language QuasiQuotes #-}
{-|
Module      : Main
Description : Day 21 solution
Copyright   : (c) Eric Mertens, 2021
License     : ISC
Maintainer  : emertens@gmail.com

<https://adventofcode.com/2015/day/21>

Buy an equipment loadout and see how it fairs against a boss in a battle.

-}
module Main (main) where

import Advent.Format (format)
import Data.List (partition)

data Item = Item { itemName :: String, itemCost, itemDamage, itemArmor :: !Int }
  deriving (Show, Read)

-- | >>> :main
-- 78
-- 148
main :: IO ()
main =
 do (hp, dmg, armor) <- [format|2015 21 Hit Points: %u%nDamage: %u%nArmor: %u%n|]
    let (wins, losses) = partition (fight hp dmg armor) gearOptions
    print (minimum (map itemCost wins))
    print (maximum (map itemCost losses))

--Weapons:    Cost  Damage  Armor
weapons :: [Item]
weapons =
  [ Item "Dagger"        8     4       0
  , Item "Shortsword"   10     5       0
  , Item "Warhammer"    25     6       0
  , Item "Longsword"    40     7       0
  , Item "Greataxe"     74     8       0
  ]

--Armor:      Cost  Damage  Armor
armors :: [Item]
armors =
  [ Item "Leather"      13     0       1
  , Item "Chainmail"    31     0       2
  , Item "Splintmail"   53     0       3
  , Item "Bandedmail"   75     0       4
  , Item "Platemail"   102     0       5
  ]

-- Rings:      Cost  Damage  Armor
rings :: [Item]
rings =
  [ Item "Damage +1"    25     1       0
  , Item "Damage +2"    50     2       0
  , Item "Damage +3"   100     3       0
  , Item "Defense +1"   20     0       1
  , Item "Defense +2"   40     0       2
  , Item "Defense +3"   80     0       3
  ]

combine :: Item -> Item -> Item
combine x y = Item
  { itemName   = itemName   x ++ " and " ++ itemName y
  , itemCost   = itemCost   x + itemCost   y
  , itemDamage = itemDamage x + itemDamage y
  , itemArmor  = itemArmor  x + itemArmor  y
  }

gearOptions :: [Item]
gearOptions =
 do weapon <- weapons
    armor  <- chooseUpTo 1 armors
    ring   <- chooseUpTo 2 rings
    return (foldl1 combine (weapon : armor ++ ring))

chooseUpTo :: Int -> [a] -> [[a]]
chooseUpTo n (x:xs) | n >= 1 = map (x:) (chooseUpTo (n-1) xs) ++ chooseUpTo n xs
chooseUpTo _ _               = [[]]

fight ::
  Int  {- ^ boss hit points -} ->
  Int  {- ^ boss damage     -} ->
  Int  {- ^ boss armor      -} ->
  Item {- ^ player weapon   -} ->
  Bool {- ^ player wins     -}
fight hp dmg armor gear = outcome 100 (max 1 (dmg - itemArmor gear)) hp (max 1 (itemDamage gear - armor))

outcome ::
  Int  {- ^ player 1 HP               -} ->
  Int  {- ^ player 1 HP lost per turn -} ->
  Int  {- ^ player 2 HP               -} ->
  Int  {- ^ player 2 HP lost per turn -} ->
  Bool {- ^ player 1 wins             -}
outcome hp1 dec1 hp2 dec2 = (hp1-1)`quot`dec1 >= (hp2-1)`quot`dec2
