module Parse (
    parseCommandTerms
    , parseScenario
) where

import Text.ParserCombinators.Parsec
import Data.Maybe

import Lib

data CommandTerm = 
    RollTerm Roll
    | ModTerm Modifier
    deriving (Show)

getRoll :: CommandTerm -> Maybe Roll
getRoll (RollTerm r) = Just r
getRoll _ = Nothing

getModifier :: CommandTerm -> Maybe Modifier
getModifier (ModTerm m) = Just m
getModifier _ = Nothing

makeScenarioFromCommandTerms :: [CommandTerm] -> Scenario
makeScenarioFromCommandTerms cs = mkScenario rs ms
    where rs = mapMaybe getRoll cs
          ms = mapMaybe getModifier cs

parseCommandTerm :: Parser CommandTerm
parseCommandTerm = parseRollCommandTerm <|> parseModifierCommandTerm

parseModifierCommandTerm :: Parser CommandTerm
parseModifierCommandTerm = do
    modifier <- parseModifier
    return $ ModTerm modifier

parseModifier :: Parser Modifier
parseModifier = 
    (string "dodge" >> return Dodge)
    <|> (string "reroll" >> return HasReroll)
    <|> (string "blizzard" >> return Blizzard)
    <|> (string "sure feet" >> return SureFeet)
    <|> (string "sf" >> return SureFeet)

parseRollCommandTerm :: Parser CommandTerm
parseRollCommandTerm = do
    roll <- parseRoll
    return $ RollTerm roll

parseRoll :: Parser Roll
parseRoll = parseDodgeRoll <|> parseGFI

-- 4+d for a four-up dodge
parseDodgeRoll :: Parser Roll
parseDodgeRoll = do
    targetDigit <- oneOf ['2'..'6'] <?> "Dodge roll needs value in range [2,6]"
    char '+'
    char 'd'
    return $ DodgeRoll $ read [targetDigit]

parseGFI :: Parser Roll
parseGFI = (string "gfi") >> return GFIRoll

parseScenario :: Parser Scenario
parseScenario = do
    commandTerms <- parseCommandTerms
    return $ makeScenarioFromCommandTerms commandTerms

parseCommandTerms :: Parser [CommandTerm]
parseCommandTerms = sepBy1 parseCommandTerm spaces
