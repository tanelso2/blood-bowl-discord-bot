import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

closeBy :: Double -> Double -> Bool
closeBy a b
    | (abs (a - b)) <= delta = True
    | otherwise = False
    where delta = 0.0001

shouldBeCloseBy :: Double -> Double -> Expectation
-- shouldBeCloseBy a b = closeBy a b `shouldBe` True
shouldBeCloseBy a b = shouldSatisfy a (closeBy b)

prop_allD6Odds :: Property
prop_allD6Odds = forAll (chooseInt (1,6)) prop_D6Odds

prop_D6Odds :: Int -> Bool
prop_D6Odds x = closeBy odds $ percentOdds $ mkScenario [DodgeRoll x] []
    where odds = (1.0 - ((fromIntegral (x-1)) / 6.0))

oddsTest :: Double -> [Roll] -> [Modifier] -> Expectation
oddsTest expected rs ms = shouldBeCloseBy expected $ percentOdds $ mkScenario rs ms

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
        prop_allD6Odds
          
