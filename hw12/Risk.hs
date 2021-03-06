{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

battle :: Battlefield -> Rand StdGen Battlefield
battle  bf(Battlefield an dn)
  | an < 2 || dn < 1 = return bf
  | otherwise = do
    attackDice <- dice (attackUnit bf)
    defendDice <- dice (defendUnit bf)
    let result = zip (sortDesc attackDice) (sortDesc defendDice)
        defendDelta = length $ filter uncurry (>) result  -- same thing as uncurry, next line below
        attackDelta = length $ filter (\(a, b) -> a <= b) result
    return bf { attackers = an - attackDelta, defenders = dn - defendDelta}

atackUnit :: Battlefield -> Army
attackUnit bf@(Battlefield an dn)
  | an <= 3 = an - 1
  | otherwise = 3

defendUnit :: Battlefield -> Army
defendUnit bf = min 2 (defenders bf)

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (flip compare)

invade :: Battlefield -> Rand StdGen Double
invade bf =
  if attackers bf < 2 || defenders bf <= 0
  then return bf
  else battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  result <-  replicateM 1000 (invade bf)
  -- mapM invade (replicate 1000 bf)
  --replicate 1000  sequence $ map invade (replicate 1000 bf)
  let winNumber = length $ filter win result
  return (fromIntegral winNumber / 1000)
  where win bf = defenders <= 0
