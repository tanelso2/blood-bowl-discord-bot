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


data Roll = DodgeRoll Int -- a dodge roll with difficulty <x>-up
    | PickupRoll Int -- a pickup roll with difficulty <x>-up
    | ThrowRoll Int -- a throw with difficulty <x>-up
    | CatchRoll Int -- a catch with difficulty <x>-up
    | GFIRoll -- a GFI
    | ProRoll Roll -- a Pro Roll to reroll a <roll>
    | LonerRoll Roll -- a Loner roll while trying to reroll x
    | ArmorCheck Int ArmorCheckCause -- armor check roll against Armor <x>
    | InjuryRoll
    | CasualtyRoll
    deriving (Show, Eq)

data ArmorCheckCause =
    FallingDown -- falling down, whether that's from a block or failing a gfi
    | Stab -- stab is another way we can trigger an armor check
    -- is it just these two? That'll work
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

type Timeline = [Result]

data Result = EndResult
    | DXRollResult Int Int
    | KnockdownResult
    | KOResult
    | StunnedResult
    | InjuryResult
    | TurnoverResult
    | CasualtyResult Casualty
    | FailureResult
    deriving (Show, Eq)

data EventTree =
    DodgeNode Int EventTree EventTree
    | YesNoDecision Decision EventTree EventTree
    | ArmorCheckNode EventTree
    | InjuryNode
    | EmptyTree

data Injury =
    Stun
    | KO
    | CasualtyInjury

-- UseSkill Skill ?
data Decision = UseDodge Int


mkTree :: Modifiers -> Rolls -> EventTree
mkTree ms [] = EmptyTree
mkTree ms ((DodgeRoll x):rs) = DodgeNode x failureTree successTree
    where failureTree = mkYesNoDecision (UseDodge x) ms rs
          successTree = mkTree ms rs

-- TODO - if hasDodge?
mkYesNoDecision :: Decision -> Modifiers -> Rolls -> EventTree
mkYesNoDecision (UseDodge x) ms rs = YesNoDecision (UseDodge x) yesTree noTree
  where yesTree = mkTree ((Rerolled):(remove Dodge ms)) ((DodgeRoll x):rs)
        noTree = ArmorCheckNode EmptyTree

data Casualty =
    BadlyHurt
    | BrokenJaw
    | BrokenRibs
    | FracturedArm
    | FracturedLeg
    | SmashedHand
    | GougedEye
    | GroinStrain
    | PinchedNerve
    | DamagedBack
    | SmashedKnee
    | SmashedAnkle
    | SmashedHip
    | FracturedSkull
    | SeriousConcussion
    | BrokenNeck
    | SmashedCollarBone
    | Dead
    deriving (Eq, Show)

-- likelihood of this timeline happening, I think?
likelihood :: Timeline -> Double
likelihood [] = 1.0
likelihood rs = foldl' (\acc x -> acc * resultLikelihood x) 1.0 rs

resultLikelihood :: Result -> Double
-- d6 roll timeline splits always have a 1/6 chance of this particular one happening
resultLikelihood (DXRollResult x _) = 1.0 / (fromIntegral x)
resultLikelihood _ = 1.0

--d6 :: [Roll] -> [Modifier] -> (Int -> [Timeline]) -> ([Timeline] -> a) -> a

calculateTimelines :: [Roll] -> [Modifier] -> [Timeline]
calculateTimelines [] _ =
    [[EndResult]]
calculateTimelines (r:rs) ms =
    case r of
        DodgeRoll _ -> dxRollCombineTimelines 6 $ timelinesD6Roll (Just Dodge) r rs ms
        PickupRoll _ -> d6RollCombineTimelines $ timelinesD6Roll (Just SureHands) r rs ms
        ThrowRoll _ -> d6RollCombineTimelines $ timelinesD6Roll  (Just Pass) r rs ms
        CatchRoll _ -> d6RollCombineTimelines $ timelinesD6Roll (Just Catch) r rs ms
        GFIRoll -> d6RollCombineTimelines $ timelinesD6Roll (Just SureFeet) r rs ms
        ProRoll r -> d6RollCombineTimelines $ timelinesProRoll rs ms r
        LonerRoll r -> d6RollCombineTimelines $ timelinesLonerRoll rs ms r
        ArmorCheck av c -> twoD6RollCombineTimelines $ timelinesArmorCheckRoll rs ms av c
        InjuryRoll -> twoD6RollCombineTimelines $ timelinesInjuryRoll ms
        CasualtyRoll -> dxRollCombineTimelines 6 $ (\x -> dxRollCombineTimelines 8 $ (\y -> timelinesCasualtyRoll (x*10+y)))

timelinesCasualtyRoll :: Int -> [Timeline]
timelinesCasualtyRoll x = singleTimeline $ [CasualtyResult (getCasualty x)]

getCasualty :: Int -> Casualty
getCasualty roll
  | roll `elem` [11..38] = BadlyHurt
  | roll == 41 = BrokenJaw
  | roll == 42 = BrokenRibs
  | roll == 43 = FracturedArm
  | roll == 44 = FracturedLeg
  | roll == 45 = SmashedHand
  | roll == 46 = GougedEye
  | roll == 47 = GroinStrain    
  | roll == 48 = PinchedNerve     
  | roll == 51 = DamagedBack
  | roll == 52 = SmashedKnee
  | roll == 53 = SmashedAnkle
  | roll == 54 = SmashedHip
  | roll == 55 = FracturedSkull
  | roll == 56 = SeriousConcussion
  | roll == 57 = BrokenNeck
  | roll == 58 = SmashedCollarBone
  | roll `elem` [61..68] = Dead


twoD6RollCombineTimelines :: (Int -> [Timeline]) -> [Timeline]
twoD6RollCombineTimelines ts = (dxRollCombineTimelines 6) $ (\x -> (dxRollCombineTimelines 6) (\y -> ts (x+y)))


timelinesInjuryRoll :: Modifiers -> Int -> [Timeline]
timelinesInjuryRoll ms roll
  | roll `elem` [2..7] = singleTimeline $ [StunnedResult, FailureResult]
  | roll `elem` [8..9] = singleTimeline $ [KOResult, FailureResult]
  | roll `elem` [10..12] = calculateTimelines (CasualtyRoll:[]) ms

singleTimeline :: Timeline -> [Timeline]
singleTimeline t = [t]

timelinesArmorCheckRoll :: Rolls -> Modifiers -> Int -> ArmorCheckCause -> Int -> [Timeline]
timelinesArmorCheckRoll rs ms av cause roll
  | roll > av = injuryRoll ms
  -- | roll > av = singleTimeline $ result:InjuryResult:FailureResult:[]
  | otherwise = singleTimeline $ armorCheckFailureResult cause

armorCheckFailureResult :: ArmorCheckCause -> Timeline
armorCheckFailureResult FallingDown = [FailureResult]
armorCheckFailureResult Stab = []

injuryRoll :: Modifiers -> [Timeline]
injuryRoll ms = calculateTimelines [InjuryRoll] ms

timelinesLonerRoll :: Rolls -> Modifiers -> Roll -> Int -> [Timeline]
timelinesLonerRoll rs ms r roll
  -- success, do the reroll
  | roll >= 4 = map (\x -> result:x) $ calculateTimelines (r:rs) $ Rerolled:ms
  -- failure
  | otherwise = failWith result r rs ms
  where result = DXRollResult 6 roll

timelinesProRoll :: Rolls -> Modifiers -> Roll -> Int -> [Timeline]
timelinesProRoll rs ms r roll
  | roll >= 4 = map (\x -> result:x) $ calculateTimelines (r:rs) $ Rerolled:(remove Pro ms)
  | hasReroll ms = map (\x -> result:x) $ useTeamReroll' rs ms (ProRoll r)
  | otherwise = failWith result r rs ms
  where result = DXRollResult 6 roll


failWith :: Result -> Roll -> Rolls -> Modifiers -> [Timeline]
failWith res r rs ms = map (res:) $ 
  case getFailureResult r of
    KnockdownResult -> calculateTimelines [(ArmorCheck 7 FallingDown)] ms
    TurnoverResult -> singleTimeline $ TurnoverResult:FailureResult:[]
    x -> error $ "failWith not implemented for" ++ show x

d6RollCombineTimelines = dxRollCombineTimelines 6

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
        result = DXRollResult 6 roll

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
    ArmorCheck _ _ -> InjuryResult


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
d6RollCombine = dxRollCombine 6 

dxRollCombine :: Int -> (Int -> a) -> (a -> b -> b) -> b -> b
dxRollCombine x func combineFunc start = foldr combineFunc start $ map func [1..x]

dxRollCombineTimelines :: Int -> (Int -> [Timeline]) -> [Timeline]
dxRollCombineTimelines x ts = (dxRollCombine x) ts' (\acc x -> acc ++ x) []
  where ts' roll = map ((DXRollResult x roll):) $ ts roll

pushDxResult :: Int -> (Int -> [Timeline]) -> (Int -> [Timeline])
pushDxResult x ts roll = map ((DXRollResult x roll):) $ timelines
  where timelines = ts roll
        


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
