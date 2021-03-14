module Lib
    ( Scenario
    , Roll(..)
    , Modifier(..)
    , mkScenario
    , percentOdds
    , simulateDodgeRoll
    , simulateTimeline
    ) where

import Data.Foldable

data Roll = DodgeRoll Int -- a dodge roll with difficulty <x>-up
    | GFIRoll
    deriving (Show)

data Modifier = 
    Dodge --dodging player has dodge skill 
    | HasReroll -- player has a reroll they can use
    | Rerolled --has this action been rerolled once already due to automatic reroll?
    | Blizzard --there's a blizzard
    | SureFeet -- player has Sure Feet skill
    deriving (Show, Eq)

mkScenario :: [Roll] -> [Modifier] -> Scenario
mkScenario rs ms = Scenario {rolls=rs, modifiers=ms}

data Scenario = Scenario {
    rolls :: [Roll]
    , modifiers :: [Modifier]
} deriving (Show)

percentOdds :: Scenario -> Double
percentOdds (Scenario { rolls = rs, modifiers = ms}) = simulateTimeline rs ms

simulateTimeline :: [Roll] -> [Modifier] -> Double
simulateTimeline [] _ = 1.0
simulateTimeline (r:rs) ms =
    case r of
        DodgeRoll x -> equalChances $ map (simulateDodgeRoll rs ms x) [1..6]
        GFIRoll -> equalChances $ map (simulateGFIRoll rs ms) [1..6]


equalChances :: [Double] -> Double
equalChances cs = sum $ map (*chance) cs
    where chance = 1.0 / (fromIntegral (length cs))

-- nextAction resets Rerolled modifier
nextAction :: [Roll] -> [Modifier] -> Double
nextAction rs ms = simulateTimeline rs $ filter (/=Rerolled) ms

simulateDodgeRoll :: [Roll] -> [Modifier] -> Int -> Int -> Double
simulateDodgeRoll rs ms target roll
  -- success, proceed on
  | roll >= target = nextAction rs ms
  -- if dodge skill, automatic reroll, use up dodge
  | Dodge `elem` ms = simulateTimeline ((DodgeRoll target):rs) $ (Rerolled):(filter (/=Dodge) ms)
  -- if player has reroll, use up reroll
  | hasReroll ms =
      simulateTimeline ((DodgeRoll target):rs) $ (Rerolled):(filter (/=HasReroll) ms)
  -- failure, probability is 0
  | otherwise = 0.0

hasReroll :: [Modifier] -> Bool
hasReroll ms = HasReroll `elem` ms && (not $ Rerolled `elem` ms)

simulateGFIRoll :: [Roll] -> [Modifier] -> Int -> Double
simulateGFIRoll rs ms roll
  -- success, proceed on
  | roll >= target = nextAction rs ms
  -- if Sure Feet skill, automatic reroll, use up SureFeet
  | SureFeet `elem` ms =
      simulateTimeline (GFIRoll:rs) $ (Rerolled):(filter (/=SureFeet) ms)
  -- if player has reroll, use up reroll
  | hasReroll ms =
      simulateTimeline (GFIRoll:rs) $ (Rerolled):(filter (/=HasReroll) ms)
  -- failure, probability is 0
  | otherwise = 0.0
  where target = getGFITarget ms

getGFITarget :: [Modifier] -> Int
getGFITarget ms
    | Blizzard `elem` ms = 3
    | otherwise = 2
     