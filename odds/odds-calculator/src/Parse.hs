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
parseModifier = choice $ map try [
    constMod "blizzard" Blizzard
    , constMod "dodge" Dodge
    , constMod "reroll" HasReroll
    , constMod "sf" SureFeet
    , constMod "sh" SureHands
    , constMod "sure feet" SureFeet
    , constMod "sure hands" SureHands
    , constMod "pass" Pass
    , constMod "catch" Catch
    , constMod "loner" Loner
    , constMod "loser" Loner
    , constMod "pro" Pro
    ]

constMod :: String -> Modifier -> Parser Modifier
constMod s x = (string s >> return x)

parseRollCommandTerm :: Parser CommandTerm
parseRollCommandTerm = do
    roll <- parseRoll
    return $ RollTerm roll

parseRoll :: Parser Roll
parseRoll = choice $ map try 
    [
    parseDodgeRoll
    , parseGFI
    , parsePickupRoll
    , parseThrowRoll
    , parseCatchRoll
    ]

-- e.g. 4+d for a four-up dodge
parseDodgeRoll :: Parser Roll
parseDodgeRoll = (do
    targetDigit <- oneOf ['2'..'6']
    char '+'
    char 'd'
    return $ DodgeRoll $ read [targetDigit]
    ) 
    <?> "[2-6]+d - dodgeRoll"

parsePickupRoll :: Parser Roll
parsePickupRoll = (do
    targetDigit <- oneOf ['2'..'6']
    char '+'
    char 'p'
    optional $ char 'u'
    return $ PickupRoll $ read [targetDigit]
    )
    <?> "[2-6]+p - pickup roll"

parseThrowRoll :: Parser Roll
parseThrowRoll = (do
    targetDigit <- oneOf ['2'..'6']
    char '+'
    char 't'
    return $ ThrowRoll $ read [targetDigit]
    )
    <?> "[2-6]+t - throw roll"

parseCatchRoll :: Parser Roll
parseCatchRoll = (do
    targetDigit <- oneOf ['2'..'6']
    char '+'
    char 'c'
    return $ CatchRoll $ read [targetDigit]
    )
    <?> "[2-6]+c - catch roll"


parseGFI :: Parser Roll
parseGFI = (string "gfi") >> return GFIRoll

parseScenario :: Parser Scenario
parseScenario = do
    commandTerms <- parseCommandTerms
    return $ makeScenarioFromCommandTerms commandTerms

parseCommandTerms :: Parser [CommandTerm]
parseCommandTerms = sepBy1 parseCommandTerm spaces
