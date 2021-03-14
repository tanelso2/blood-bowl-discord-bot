module Main where

import Lib

main :: IO ()
main = print $ percentOdds $ mkScenario [DodgeRoll 4] []