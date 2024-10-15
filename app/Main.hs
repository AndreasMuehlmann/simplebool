{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Main where

import System.Environment
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Either (fromRight)


data Token = CONJUNCTION
           | DISJUNCTION
           | NEGATION
           | LBRACKET
           | RBRACKET
           | CONSTANT Bool
           | IDENTIFIER String
           deriving(Show, Eq)

data BoolExpr = And BoolExpr BoolExpr
                    | Or BoolExpr BoolExpr
                    | Negation BoolExpr
                    | Variable String
                    | Constant Bool
                    deriving(Show, Eq)

identifierToToken :: String -> Token
identifierToToken string
    | string == "and" = CONJUNCTION
    | string == "or" = DISJUNCTION
    | string == "not" = NEGATION
    | otherwise = IDENTIFIER string

getIdentifier :: String -> Either String (String, String)
getIdentifier [] = Right ("", "")
getIdentifier (x:xs)
    | isSpace x || isJust token  = Right ("", x:xs)
    | isLetter x || isDigit x = do
                           (identifier, remainder) <- getIdentifier xs
                           Right (x : identifier, remainder)
    | otherwise = Left "Unexpected token"
    where token = charToToken x

charToToken :: Char -> Maybe Token
charToToken '(' = Just LBRACKET
charToToken ')' = Just RBRACKET
charToToken '0' = Just (CONSTANT False)
charToToken '1' = Just (CONSTANT True)
charToToken char = Nothing

tokenize :: [Token] -> String -> Either String [Token]
tokenize accumulator [] = Right accumulator
tokenize accumulator (x:xs)
    | isSpace x = tokenize accumulator xs
    | isJust maybeToken = tokenize (accumulator ++ [fromJust maybeToken]) xs
    | otherwise = do
        (identifier, remainder) <- getIdentifier (x:xs)
        tokens <- tokenize (accumulator ++ [identifierToToken identifier]) remainder
        Right tokens
    where maybeToken = charToToken x

splitAtToken :: [Token] -> Token -> Maybe ([Token], [Token])
splitAtToken [] token = Nothing
splitAtToken (x:xs) token
    | token == x = Just ([], xs)
    | otherwise = do
        (before, after) <- splitAtToken xs token
        Just (x : before, after)

tokenToBoolExpr :: Token -> Either String BoolExpr 
tokenToBoolExpr (CONSTANT bool) = Right $ Constant bool
tokenToBoolExpr (IDENTIFIER string) = Right $ Variable string
tokenToBoolExpr token = Left "Unexpected token"

parseBinaryOperator :: Token -> [Token] -> [Token] -> Either String BoolExpr
parseBinaryOperator operator left right = do
                                parsedLeft <- parse left
                                parsedRight <- parse right
                                case operator of
                                    CONJUNCTION -> Right $ And parsedLeft parsedRight
                                    DISJUNCTION -> Right $ Or parsedLeft parsedRight

parse :: [Token] -> Either String BoolExpr
parse [] = Left "Operator is missing a value."
parse [token] = tokenToBoolExpr token
parse (x:y:xs) = parseBinaryOperator y [x] xs


main :: IO ()
main = do
    args <- getArgs;
    if null args
    then print "Error: An argument with the boolean expression to be simplified is required."
    else do
        let tokensResult = tokenize [] $ head args
        either print (print . parse) tokensResult
