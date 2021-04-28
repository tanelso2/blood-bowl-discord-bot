import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

closeBy :: Double -> Double -> Bool
closeBy a b
    | (abs (a - b)) <= delta = True
    | otherwise = False
    where delta = 0.0000001

shouldBeCloseBy :: Double -> Double -> Expectation
shouldBeCloseBy a b = (a, b) `shouldSatisfy` (uncurry closeBy)

allD6 :: (Int -> Bool) -> Property
allD6 p = forAll (chooseInt (1, 6)) p

prop_D6Odds :: Int -> Bool
prop_D6Odds x = closeBy odds $ percentOdds $ mkScenario [DodgeRoll x] []
    where odds = rollSuccessOdds x

prop_D6WithRerollOdds :: Int -> Bool
prop_D6WithRerollOdds x = closeBy odds $ percentOdds $ mkScenario [DodgeRoll x] [HasReroll]
    where odds = rollSuccessOdds x + ((rollFailureOdds x) * (rollSuccessOdds x))

prop_D6ChainingRolls :: [Int] -> Bool
prop_D6ChainingRolls xs = closeBy odds $ percentOdds $ mkScenario rolls []
    where rolls = map DodgeRoll xs
          odds = foldr (\x acc -> acc * rollSuccessOdds x) 1.0 xs

prop_TimelineProbabilitySum :: [Roll] -> [Modifier] -> Expectation
prop_TimelineProbabilitySum rs ms = shouldBeCloseBy actual expected
    where expected = 1.0
          actual = foldr (\x acc -> acc + (likelihood x)) 0.0 $ calculateTimelines rs ms

prop_TimelineAndFirstFunctionAgree :: [Roll] -> [Modifier] -> Expectation
prop_TimelineAndFirstFunctionAgree rs ms = shouldBeCloseBy r1 r2
    where
      scenario = mkScenario rs ms
      r1 = percentOdds scenario
      r2 = percentOdds' scenario

-- an x+ role has what chance of success
rollSuccessOdds :: Int -> Double
rollSuccessOdds x = (1.0 - ((fromIntegral (x-1)) /6.0))

-- an x+ role has what chance of failure
rollFailureOdds :: Int -> Double
rollFailureOdds x = (1.0 - (rollSuccessOdds x))

oddsTest :: Double -> [Roll] -> [Modifier] -> Expectation
oddsTest expected rs ms = shouldBeCloseBy expected $ percentOdds $ mkScenario rs ms

forShortRandomScenarios :: Testable prop => ([Roll] -> [Modifier] -> prop) -> Property
forShortRandomScenarios p = forAll randomRolls (\rs -> forAll randomModifiers (\ms -> p rs ms))

instance Arbitrary Modifier where
  arbitrary = randomModifier

instance Arbitrary Roll where
  arbitrary = do
    x <- chooseInt (1,6)
    return $ DodgeRoll x

randomModifier :: Gen Modifier
randomModifier = chooseAny

maxsize = 6

randomModifiers :: Gen [Modifier]
randomModifiers = resize maxsize $ listOf randomModifier

randomRolls :: Gen [Roll]
randomRolls = resize maxsize $ listOf $ arbitrary

main :: IO ()
main = hspec $ do
  describe "probability" $ do
    describe "Dodge rolls" $ do
      it "4-up dodge roll" $ do
        oddsTest 0.5 [DodgeRoll 4] []
      it "4-up dodge roll w/ dodge" $ do
        oddsTest 0.75 [DodgeRoll 4] [Dodge]
      it "4-up dodge roll w/ team reroll" $ do
        oddsTest 0.75 [DodgeRoll 4] [HasReroll]
      it  "2-up dodge" $ do
        oddsTest (5.0 /6.0) [DodgeRoll 2] []
      it "4-up dodge w/ irrelevant skill" $ do
        oddsTest 0.5 [DodgeRoll 4] [Catch]
    describe "GFI Rolls" $ do
      it "gfi" $ do
        oddsTest (5.0 / 6.0) [GFIRoll] []
      it "gfi in blizzard" $ do
        oddsTest (2.0 / 3.0) [GFIRoll] [Blizzard]
    describe "Best odds" $ do
      it "Should use dodge over pro - better odds" $ do
        oddsTest 0.75 [DodgeRoll 4] [Dodge, Pro]
    it "Checking the odds of <x>+ dodgerolls with no modifiers" $ 
        allD6 prop_D6Odds
    it "Checking the odds of <x>+ dodgerolls with a reroll" $
        allD6 prop_D6WithRerollOdds
    it "Checking the odds of two random dodgerolls in a row" $
        forAll (chooseInt (1,6)) (\x -> forAll (chooseInt (1, 6)) (\y -> prop_D6ChainingRolls [x,y]))
    it "Random dodgerolls in a row" $ property $ do
        xs <- resize 10 $ listOf $ chooseInt (1, 6)
        return $ prop_D6ChainingRolls xs
  describe "Timelines" $ do
    xit "Probabilities should sum up to 1.0" $ property $
      forShortRandomScenarios $ prop_TimelineProbabilitySum
    xit "Sum to one debugging" $ do
      ts <- return $ calculateTimelines [DodgeRoll 4] [Pro, Loner, Dodge]
      print $ show $ ts
    xit "The two functions should agree" $ property $
      forShortRandomScenarios $ prop_TimelineAndFirstFunctionAgree
