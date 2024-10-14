module Main where

import System.Environment
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (isJust, isNothing, fromJust)


data Token = CONJUNCTION
           | DISJUNCTION
           | NEGATION
           | LBRACKET
           | RBRACKET
           | CONSTANT Bool
           | IDENTIFIER String
           deriving(Show)

data BoolExpression = And BoolExpression BoolExpression
                    | Or BoolExpression BoolExpression
                    | Negation BoolExpression
                    | Variable String
                    | Constant Bool
                    deriving(Show)

identifierToToken :: String -> Token
identifierToToken string
    | string == "and" = CONJUNCTION
    | string == "or" = DISJUNCTION
    | string == "not" = NEGATION
    | otherwise = IDENTIFIER string

getIdentifier :: String -> Maybe (String, String)
getIdentifier [] = Just ("", "")
getIdentifier (x:xs)
    | isSpace x || isJust token  = Just ("", x:xs)
    | isLetter x || isDigit x = case getIdentifier xs of
        Just (identifier, remainder) -> Just (x : identifier, remainder)
        Nothing -> Nothing
    where token = charToToken x

charToToken :: Char -> Maybe Token
charToToken '(' = Just LBRACKET
charToToken ')' = Just RBRACKET
charToToken '0' = Just (CONSTANT False)
charToToken '1' = Just (CONSTANT True)
charToToken char = Nothing

tokenize :: [Token] -> String -> Either [Token] String
tokenize accumulator [] = Left accumulator
tokenize accumulator (x:xs)
    | isSpace x = tokenize accumulator xs
    | isJust maybeToken = tokenize (accumulator ++ [fromJust maybeToken]) xs
    | otherwise = case getIdentifier (x:xs) of
        Just (identifier, remainder) -> tokenize (accumulator ++ [identifierToToken identifier]) remainder
        Nothing -> Right "Getting identifier impossible."
    where maybeToken = charToToken x

main :: IO ()
main = do
    args <- getArgs;
    if null args
    then print "Error: An argument with the boolean expression to be simplified is required."
    else do
        let tokensResult = tokenize [] $ head args
        either print print tokensResult
