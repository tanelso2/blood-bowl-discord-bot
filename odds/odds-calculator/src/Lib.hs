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
    | PickupRoll Int -- a pickup roll with difficulty <x>-up
    | ThrowRoll Int -- a throw with difficulty <x>-up
    | CatchRoll Int -- a catch with difficulty <x>-up
    | GFIRoll -- a GFI
    deriving (Show)

-- TODO: Pro
data Modifier = 
    Dodge --dodging player has dodge skill 
    | HasReroll -- player has a reroll they can use
    | Rerolled --has this action been rerolled once already due to automatic reroll?
    | Blizzard --there's a blizzard
    | SureFeet -- player has Sure Feet skill
    | SureHands -- player has Sure Hands skill
    | Pass -- player has Pass skill
    | Catch -- player has Catch skill
    | Loner -- player has Loner skill
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
        PickupRoll x -> equalChances $ map (simulatePickupRoll rs ms x) [1..6]
        ThrowRoll x -> equalChances $ map (simulateThrowRoll rs ms x) [1..6]
        CatchRoll x -> equalChances $ map (simulateCatchRoll rs ms x) [1..6]

useTeamReroll :: [Roll] -> [Modifier] -> Roll -> Double
useTeamReroll rs ms x
    | Loner `elem` ms = equalChances [doReroll, 0.0]
    | otherwise = doReroll
    where doReroll = simulateTimeline (x:rs) $ (Rerolled):(remove HasReroll ms)

equalChances :: [Double] -> Double
equalChances cs = sum $ map (*chance) cs
    where chance = 1.0 / (fromIntegral (length cs))

-- nextAction resets Rerolled modifier
nextAction :: [Roll] -> [Modifier] -> Double
nextAction rs ms = simulateTimeline rs $ remove Rerolled ms

remove :: (Eq a) => a -> [a] -> [a]
remove x xs = filter (/=x) xs

simulateDodgeRoll :: [Roll] -> [Modifier] -> Int -> Int -> Double
simulateDodgeRoll rs ms target roll
  -- success, proceed on
  | roll >= target = nextAction rs ms
  -- if dodge skill, automatic reroll, use up dodge
  | Dodge `elem` ms = simulateTimeline ((DodgeRoll target):rs) $ (Rerolled):(remove Dodge ms)
  -- if player has reroll, use up reroll
  | hasReroll ms =
      useTeamReroll rs ms (DodgeRoll target)
  -- failure, probability is 0
  | otherwise = 0.0

simulateCatchRoll :: [Roll] -> [Modifier] -> Int -> Int -> Double
simulateCatchRoll rs ms target roll
  -- success, proceed on
  | roll >= target = nextAction rs ms
  -- if catch skill, automatic reroll, use up catch 
  | Catch `elem` ms = simulateTimeline ((CatchRoll target):rs) $ (Rerolled):(remove Catch ms)
  -- if player has reroll, use up reroll
  | hasReroll ms =
      useTeamReroll rs ms $ CatchRoll target
  -- failure, probability is 0
  | otherwise = 0.0

simulateThrowRoll :: [Roll] -> [Modifier] -> Int -> Int -> Double
simulateThrowRoll rs ms target roll
  -- success, proceed on
  | roll >= target = nextAction rs ms
  -- if pass skill, automatic reroll, use up pass 
  | Pass `elem` ms = simulateTimeline ((ThrowRoll target):rs) $ (Rerolled):(remove Pass ms)
  -- if player has reroll, use up reroll
  | hasReroll ms =
      useTeamReroll rs ms $ ThrowRoll target
  -- failure, probability is 0
  | otherwise = 0.0

simulatePickupRoll :: [Roll] -> [Modifier] -> Int -> Int -> Double
simulatePickupRoll rs ms target roll
  -- success, proceed on
  | roll >= target = nextAction rs ms
  -- if SureHands skill, automatic reroll, use up Sure Hands
  | SureHands `elem` ms = simulateTimeline ((PickupRoll target):rs) $ (Rerolled):(remove SureHands ms)
  -- if player has reroll, use up reroll
  | hasReroll ms =
      useTeamReroll rs ms $ PickupRoll target
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
      useTeamReroll rs ms GFIRoll
  -- failure, probability is 0
  | otherwise = 0.0
  where target = getGFITarget ms

getGFITarget :: [Modifier] -> Int
getGFITarget ms
    | Blizzard `elem` ms = 3
    | otherwise = 2
     