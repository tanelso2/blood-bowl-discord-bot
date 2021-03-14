module Main where

import Text.ParserCombinators.Parsec
import System.IO

import Lib
import Parse

main :: IO ()
main = do
    input <- getLine
    either handleFailure handleSuccess $ parse parseScenario "" (input::String)

handleFailure x = do
    putStrLn "ERROR"
    print x

handleSuccess :: Scenario -> IO ()
handleSuccess scenario = do
    hPutStrLn stderr $ "Scenario parsed as " ++ (show scenario)
    percentage <- return $ percentOdds scenario
    putStrLn $ "The chance of that is " ++ show percentage