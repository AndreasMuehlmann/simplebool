{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Main where

import System.Environment
import Data.Char (isSpace, isLetter, isDigit)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Either (fromRight, isRight)


data Token = CONJUNCTION
           | DISJUNCTION
           | NEGATION
           | LBRACKET
           | RBRACKET
           | CONSTANT Bool
           | IDENTIFIER String
           deriving(Eq)

instance Show Token where
    show CONJUNCTION = "and"
    show DISJUNCTION = "or"
    show (IDENTIFIER name) = name
    show (CONSTANT True) = "1"
    show (CONSTANT False) = "0"


data BoolExpr = And BoolExpr BoolExpr
                    | Or BoolExpr BoolExpr
                    | Negation BoolExpr
                    | Variable String
                    | Constant Bool
                    deriving(Eq)

instance Show BoolExpr where
    show (And leftExpr rightExpr) = (if bracketsRequired leftExpr then "(" ++ show leftExpr ++ ")" else show leftExpr)
                                    ++ " and " ++ (if bracketsRequired rightExpr then "(" ++ show rightExpr ++ ")" else show rightExpr)
    show (Or leftExpr rightExpr) = (if bracketsRequired leftExpr then "(" ++ show leftExpr ++ ")" else show leftExpr)
                                   ++ " or " ++ (if bracketsRequired rightExpr then "(" ++ show rightExpr ++ ")" else show rightExpr)
    show (Negation expr) = if bracketsRequired expr then "not (" ++ show expr ++ ")" else "not " ++ show expr
    show (Variable name) = name
    show (Constant True) = "1"
    show (Constant False) = "0"

bracketsRequired :: BoolExpr -> Bool
bracketsRequired (And x y) = True
bracketsRequired (Or x y) = True
bracketsRequired expr = False

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

splitAtMatchingBracket :: [Token] -> Int -> Maybe ([Token], [Token])
splitAtMatchingBracket [] bracketCount = Nothing
splitAtMatchingBracket (x:xs) bracketCount
    | RBRACKET == x && bracketCount == 1 = Just ([], xs)
    | otherwise = do
        (before, after) <- splitAtMatchingBracket xs $ case x of
                                                        LBRACKET -> bracketCount + 1
                                                        RBRACKET -> bracketCount - 1
                                                        _ -> bracketCount
        Just (x : before, after)

tokenToBoolExpr :: Token -> Either String BoolExpr
tokenToBoolExpr (CONSTANT bool) = Right $ Constant bool
tokenToBoolExpr (IDENTIFIER string) = Right $ Variable string
tokenToBoolExpr token = Left "Token is not BoolExpr"

parseBoolExprWithRemainder :: Int -> BoolExpr -> [Token] -> Either String BoolExpr
parseBoolExprWithRemainder tokenIndex leftBoolExpr [] = Right leftBoolExpr
parseBoolExprWithRemainder tokenIndex leftBoolExpr remainder = do
                                            let binaryOperator = head remainder
                                            rightBoolExpr <- parse (tokenIndex + 1) (tail remainder)
                                            case binaryOperator of
                                                CONJUNCTION -> Right $ And leftBoolExpr rightBoolExpr
                                                DISJUNCTION -> Right $ Or leftBoolExpr rightBoolExpr

parseBracket :: Int -> [Token] -> Either String (BoolExpr, [Token], Int)
parseBracket tokenIndex tokens = case splitAtMatchingBracket tokens 1 of
                        Nothing -> Left "No Matching Bracket"
                        Just (toMatchingBracket, remainder) -> do
                                                            boolExprBrackets <- parse (tokenIndex + length toMatchingBracket + 1) toMatchingBracket
                                                            Right (boolExprBrackets, remainder, tokenIndex + length toMatchingBracket + 1)

parseOperand :: Int -> [Token] -> Either String (BoolExpr, [Token], Int)
parseOperand tokenIndex [] = Left "Parsing empty list of tokens to boolean expression is impossible."
parseOperand tokenIndex [token] = do
                    boolExpr <- tokenToBoolExpr token
                    Right (boolExpr, [], tokenIndex + 1)
parseOperand tokenIndex (x:xs)
    | x == NEGATION = do
                    (boolExpr, remainder, newTokenIndex) <- parseOperand (tokenIndex + 1) xs
                    Right (Negation boolExpr, remainder, newTokenIndex)
    | isRight $ tokenToBoolExpr x = do
                                tokenBoolExpr <- tokenToBoolExpr x
                                Right (tokenBoolExpr, xs, tokenIndex + 1)
    | x == LBRACKET = parseBracket (tokenIndex + 1) xs
    | otherwise = Left $ "Unexpected token \"" ++ show x ++ "\" at position " ++ show (tokenIndex + 1) ++ ". Expected operand."

parse :: Int -> [Token] -> Either String BoolExpr
parse tokenIndex tokens = do
            (operand, remainder, newTokenIndex) <- parseOperand tokenIndex tokens
            parseBoolExprWithRemainder newTokenIndex operand remainder

toAbstractSyntaxTree :: String -> Either String BoolExpr
toAbstractSyntaxTree input = do
        tokens <- tokenize [] input
        abstractSyntaxTree <- parse 0 tokens
        Right abstractSyntaxTree

main :: IO ()
main = do
    args <- getArgs;
    if null args
    then print "Error: An argument with the boolean expression to be simplified is required."
    else do
        let result = toAbstractSyntaxTree $ head args
        case result of
            Right abstractSyntaxTree -> print abstractSyntaxTree
            Left err -> putStrLn err
