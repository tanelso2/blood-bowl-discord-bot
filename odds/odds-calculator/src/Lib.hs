module Lib
    ( Scenario
    , Roll(..)
    , Modifier(..)
    , emptyScenario
    , mkScenario
    , percentOdds
    , simulateD6Roll
    , simulateTimeline
    ) where

import Data.Foldable

data Roll = DodgeRoll Int -- a dodge roll with difficulty <x>-up
    | PickupRoll Int -- a pickup roll with difficulty <x>-up
    | ThrowRoll Int -- a throw with difficulty <x>-up
    | CatchRoll Int -- a catch with difficulty <x>-up
    | GFIRoll -- a GFI
    | ProRoll Roll -- a Pro Roll to reroll a <roll>
    deriving (Show)

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
    | Pro -- player has Pro skill
    deriving (Show, Eq)

mkScenario :: [Roll] -> [Modifier] -> Scenario
mkScenario rs ms = Scenario {rolls=rs, modifiers=ms}

data Scenario = Scenario {
    rolls :: [Roll]
    , modifiers :: [Modifier]
} deriving (Show)

emptyScenario = Scenario { 
    rolls = [], 
    modifiers = []
}

percentOdds :: Scenario -> Double
percentOdds (Scenario { rolls = rs, modifiers = ms}) = simulateTimeline rs ms

simulateTimeline :: [Roll] -> [Modifier] -> Double
simulateTimeline [] _ = 1.0
simulateTimeline (r:rs) ms =
    case r of
        DodgeRoll x -> d6Roll $ simulateD6Roll (return Dodge) r rs ms x
        GFIRoll -> d6Roll $ simulateD6Roll (return SureFeet) r rs ms (getGFITarget ms)
        PickupRoll x -> d6Roll $ simulateD6Roll (return SureHands) r rs ms x
        ThrowRoll x -> d6Roll $ simulateD6Roll (return Pass) r rs ms x
        CatchRoll x -> d6Roll $ simulateD6Roll (return Catch) r rs ms x
        ProRoll r -> d6Roll $ simulateProRoll rs ms r

simulateProRoll :: [Roll] -> [Modifier] -> Roll -> Int -> Double
simulateProRoll rs ms r roll
    -- use up pro, queue up rerolling the roll, and add Rerolled to this action
    | roll >= 4 = simulateTimeline (r:rs) $ Rerolled:(remove Pro ms)
    -- Try to reroll the pro roll
    | hasReroll ms = useTeamReroll rs ms (ProRoll r)
    -- failure
    | otherwise = 0.0

d6Roll :: (Int -> Double) -> Double
d6Roll simulateFunc = equalChances $ map simulateFunc [1..6]

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

simulateD6Roll :: Maybe Modifier -> Roll -> [Roll] -> [Modifier] -> Int -> Int -> Double
-- simulateD6Roll autoRerollSkill currentRoll rs ms target roll = probability
simulateD6Roll maybeAutoSkill currentRoll rs ms target roll
  -- success, proceed on
  | roll >= target = nextAction rs ms
  -- Pro comes before all other skills
  | hasPro ms = simulateTimeline ((ProRoll currentRoll):rs) $ (remove Pro ms)
  -- if autoReroll skill, automatic reroll, use up skill
  | hasAutoRerollSkill ms maybeAutoSkill = simulateTimeline (currentRoll:rs) $ (Rerolled):(removeSkill ms maybeAutoSkill)
  -- if player has reroll, use up reroll
  | hasReroll ms = useTeamReroll rs ms currentRoll
  -- failure, probability is 0
  | otherwise = 0.0

hasPro :: [Modifier] -> Bool
hasPro ms = Pro `elem` ms

removeSkill :: [Modifier] -> Maybe Modifier -> [Modifier]
removeSkill ms maybeSkill = maybe ms (\x -> (remove x ms)) maybeSkill

hasAutoRerollSkill :: [Modifier] -> Maybe Modifier -> Bool
hasAutoRerollSkill ms maybeSkill = maybe False (\x -> x `elem` ms && notRerolled ms) maybeSkill

notRerolled :: [Modifier] -> Bool
notRerolled ms = not $ Rerolled `elem` ms

hasReroll :: [Modifier] -> Bool
hasReroll ms = HasReroll `elem` ms && notRerolled ms

getGFITarget :: [Modifier] -> Int
getGFITarget ms
    | Blizzard `elem` ms = 3
    | otherwise = 2
