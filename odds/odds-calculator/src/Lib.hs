module Lib
    ( Scenario
    , Result(..)
    , Timeline
    , Roll(..)
    , Modifier(..)
    , emptyScenario
    , mkScenario
    , percentOdds
    , percentOdds'
    , simulateD6Roll
    , simulateTimeline
    , calculateTimelines
    , likelihood
    ) where

import Data.Foldable
import System.Random
import System.Random.Internal

type Timeline = [Result]

-- likelihood of this timeline happening, I think?
likelihood :: Timeline -> Double
likelihood [] = 1.0
likelihood rs = foldl' (\acc x -> acc * resultLikelihood x) 1.0 rs

resultLikelihood :: Result -> Double
-- d6 roll timeline splits always have a 1/6 chance of this particular one happening
resultLikelihood (D6RollResult _ _ _) = (1.0/6.0)
resultLikelihood (TwoD6RollResult _ _ _) = (1.0/36.0)
resultLikelihood _ = 1.0

data Result = D6RollResult Roll Int Int
    | EndResult
    | TwoD6RollResult Roll Int Int
    | KnockdownResult
    | InjuryResult
    | TurnoverResult
    | FailureResult
    deriving (Show, Eq)


--d6 :: [Roll] -> [Modifier] -> (Int -> [Timeline]) -> ([Timeline] -> a) -> a

calculateTimelines :: [Roll] -> [Modifier] -> [Timeline]
calculateTimelines [] _ =
    [[EndResult]]
calculateTimelines (r:rs) ms =
    case r of
        DodgeRoll _ -> d6RollCombineTimelines $ timelinesD6Roll (Just Dodge) r rs ms
        PickupRoll _ -> d6RollCombineTimelines $ timelinesD6Roll (Just SureHands) r rs ms
        ThrowRoll _ -> d6RollCombineTimelines $ timelinesD6Roll  (Just Pass) r rs ms
        CatchRoll _ -> d6RollCombineTimelines $ timelinesD6Roll (Just Catch) r rs ms
        GFIRoll -> d6RollCombineTimelines $ timelinesD6Roll (Just SureFeet) r rs ms
        ProRoll r -> d6RollCombineTimelines $ timelinesProRoll rs ms r
        LonerRoll r -> d6RollCombineTimelines $ timelinesLonerRoll rs ms r
        ArmorCheck av -> d6RollCombineTimelines (\x -> d6RollCombineTimelines (\y -> timelinesArmorCheckRoll rs ms av (x+y)))

singleTimeline :: Timeline -> [Timeline]
singleTimeline t = [t]

timelinesArmorCheckRoll :: Rolls -> Modifiers -> Int -> Int -> [Timeline]
timelinesArmorCheckRoll rs ms av roll
  | roll > av = singleTimeline $ result:InjuryResult:FailureResult:[]
  | otherwise = singleTimeline $ result:KnockdownResult:FailureResult:[]
  where result = TwoD6RollResult (ArmorCheck av) av roll

timelinesLonerRoll :: Rolls -> Modifiers -> Roll -> Int -> [Timeline]
timelinesLonerRoll rs ms r roll
  -- success, do the reroll
  | roll >= 4 = map (\x -> result:x) $ calculateTimelines (r:rs) $ Rerolled:ms
  -- failure
  | otherwise = [result:(getFailureResult r):FailureResult:[]]
  where result = D6RollResult r 4 roll

timelinesProRoll :: Rolls -> Modifiers -> Roll -> Int -> [Timeline]
timelinesProRoll rs ms r roll
  | roll >= 4 = map (\x -> result:x) $ calculateTimelines (r:rs) $ Rerolled:(remove Pro ms)
  | hasReroll ms = map (\x -> result:x) $ useTeamReroll' rs ms (ProRoll r)
  | otherwise = failWith r
  where result = D6RollResult r 4 roll


failWith :: Result -> Roll -> [Timeline]
-- TODO: the type of failure should branch. Perhaps this function needs more arguments?
failWith res r = map (res:) $ singleTimeline $ [getFailureResult r, FailureResult]

d6RollCombineTimelines ts = d6RollCombine ts (\acc x -> acc ++ x) []

successfulTimeline :: Timeline -> Bool
successfulTimeline rs = not $ FailureResult `elem` rs

percentOdds' :: Scenario -> Double
percentOdds' (Scenario { rolls = rs, modifiers = ms}) = odds
    where timelines = calculateTimelines rs ms
          successfulTimelines = filter successfulTimeline timelines
          odds = sum $ map likelihood successfulTimelines

timelinesD6Roll :: Maybe Modifier -> Roll -> [Roll] -> [Modifier] -> Int -> [Timeline]
timelinesD6Roll autoRerollSkill currentRoll rs ms roll
  -- action succeeds, proceed
  | roll >= target = map (\x -> result:x) $ nextTimelineAction rs ms
  -- use auto reroll skill
  | hasAutoRerollSkill ms autoRerollSkill = map (\x -> result:x) $ calculateTimelines (currentRoll:rs) $ (Rerolled):(removeSkill ms autoRerollSkill)
  | hasReroll ms = map (\x -> result:x) $ useTeamReroll' rs ms currentRoll
  | hasPro ms = map (\x -> result:x) $ calculateTimelines ((ProRoll currentRoll):rs) $ (remove Pro ms)
  -- failure
  | otherwise = [[result, FailureResult]]
  where target = getTarget currentRoll ms
        result = D6RollResult currentRoll target roll

useTeamReroll' :: Rolls -> Modifiers -> Roll -> [Timeline]
useTeamReroll' rs ms r
  -- LONER goes here
  | Loner `elem` ms = calculateTimelines ((LonerRoll r):rs) $ remove HasReroll ms
  | otherwise = calculateTimelines (r:rs) $ (Rerolled):(remove HasReroll ms)

nextTimelineAction :: Rolls -> Modifiers -> [Timeline]
nextTimelineAction rs ms = calculateTimelines rs $ remove Rerolled ms

-- simulateD6Roll :: Maybe Modifier -> Roll -> [Roll] -> [Modifier] -> Int -> Int -> Double
-- -- simulateD6Roll autoRerollSkill currentRoll rs ms target roll = probability
-- simulateD6Roll maybeAutoSkill currentRoll rs ms target roll


type Modifiers = [Modifier]
type Rolls = [Roll]

getTarget :: Roll -> Modifiers -> Int
getTarget (DodgeRoll x) _ = x
getTarget (PickupRoll x) _ = x
getTarget (ThrowRoll x) _ = x
getTarget (CatchRoll x) _ = x
getTarget GFIRoll ms = getGFITarget ms

getFailureResult :: Roll -> Result
getFailureResult r =
  case r of
    DodgeRoll _ -> KnockdownResult
    CatchRoll _ -> TurnoverResult
    PickupRoll _ -> TurnoverResult
    ThrowRoll _ -> TurnoverResult
    GFIRoll -> KnockdownResult
    ProRoll r -> getFailureResult r
    LonerRoll r -> getFailureResult r
    ArmorCheck _ -> InjuryResult

data Roll = DodgeRoll Int -- a dodge roll with difficulty <x>-up
    | PickupRoll Int -- a pickup roll with difficulty <x>-up
    | ThrowRoll Int -- a throw with difficulty <x>-up
    | CatchRoll Int -- a catch with difficulty <x>-up
    | GFIRoll -- a GFI
    | ProRoll Roll -- a Pro Roll to reroll a <roll>
    | LonerRoll Roll -- a Loner roll while trying to reroll x
    | ArmorCheck Int -- armor check roll against Armor <x>
    deriving (Show, Eq)

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

allModifiers = [
  Dodge
  , HasReroll
  , Blizzard
  , SureFeet
  , SureHands
  , Pass
  , Catch
  , Loner
  , Pro
  ]

instance Uniform Modifier where
    uniformM g = do
        x <- uniformWord32R (fromIntegral (l-1)) g
        return $ allModifiers !! (fromIntegral x)
      where l = length allModifiers

instance Random Modifier where
    randomR (_,_) g =
      runStateGen g uniformM

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

d6RollCombine :: (Int -> a) -> (a -> b -> b) -> b -> b
d6RollCombine func combineFunc start = foldr combineFunc start $ map func [1..6]

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
  -- if autoReroll skill, automatic reroll, use up skill
  | hasAutoRerollSkill ms maybeAutoSkill = simulateTimeline (currentRoll:rs) $ (Rerolled):(removeSkill ms maybeAutoSkill)
  -- if player has reroll, use up reroll
  | hasReroll ms = useTeamReroll rs ms currentRoll
  -- Pro comes after all other skills, it helps less than them
  | hasPro ms = simulateTimeline ((ProRoll currentRoll):rs) $ (remove Pro ms)
  -- failure, probability is 0
  | otherwise = 0.0

hasPro :: [Modifier] -> Bool
hasPro ms = Pro `elem` ms && notRerolled ms

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
