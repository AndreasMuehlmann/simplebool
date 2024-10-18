module Parse where

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
    show NEGATION = "not"
    show LBRACKET = "("
    show RBRACKET = ")"
    show (IDENTIFIER name) = name
    show (CONSTANT True) = "1"
    show (CONSTANT False) = "0"

data BoolExpr = Conjunction BoolExpr BoolExpr
              | Disjunction BoolExpr BoolExpr
              | Negation BoolExpr
              | Variable String
              | Constant Bool
              deriving(Eq)

instance Show BoolExpr where
    show (Conjunction leftExpr rightExpr) = (if bracketsRequired leftExpr then "(" ++ show leftExpr ++ ")" else show leftExpr)
                                    ++ " and " ++ (if bracketsRequired rightExpr then "(" ++ show rightExpr ++ ")" else show rightExpr)
    show (Disjunction leftExpr rightExpr) = (if bracketsRequired leftExpr then "(" ++ show leftExpr ++ ")" else show leftExpr)
                                   ++ " or " ++ (if bracketsRequired rightExpr then "(" ++ show rightExpr ++ ")" else show rightExpr)
    show (Negation expr) = if bracketsRequired expr then "not (" ++ show expr ++ ")" else "not " ++ show expr
    show (Variable name) = name
    show (Constant True) = "1"
    show (Constant False) = "0"

bracketsRequired :: BoolExpr -> Bool
bracketsRequired (Conjunction x y) = True
bracketsRequired (Disjunction x y) = True
bracketsRequired expr = False

identifierToToken :: String -> Token
identifierToToken string
    | string == "and" = CONJUNCTION
    | string == "or" = DISJUNCTION
    | string == "not" = NEGATION
    | otherwise = IDENTIFIER string

getIdentifier :: Int -> String -> Either String (String, String)
getIdentifier index [] = Right ("", "")
getIdentifier index (x:xs)
    | isSpace x || isJust token  = Right ("", x:xs)
    | isLetter x || isDigit x = do
                           (identifier, remainder) <- getIdentifier (index + 1) xs
                           Right (x : identifier, remainder)
    | otherwise = Left $ "Unexpected character \"" ++ [x] ++ "\" at position " ++ show (index + 1) ++ ". Expected letter or digit in identifier."
    where token = charToToken x

charToToken :: Char -> Maybe Token
charToToken '(' = Just LBRACKET
charToToken ')' = Just RBRACKET
charToToken '0' = Just (CONSTANT False)
charToToken '1' = Just (CONSTANT True)
charToToken char = Nothing

tokenize :: Int -> [Token] -> String -> Either String [Token]
tokenize index accumulator [] = Right accumulator
tokenize index accumulator (x:xs)
    | isSpace x = tokenize (index + 1) accumulator xs
    | isJust maybeToken = tokenize (index + 1) (accumulator ++ [fromJust maybeToken]) xs
    | otherwise = do
        (identifier, remainder) <- getIdentifier index (x:xs)
        tokens <- tokenize (length identifier) (accumulator ++ [identifierToToken identifier]) remainder
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

tokenToBoolExpr :: Int -> Token -> Either String BoolExpr
tokenToBoolExpr tokenIndex (CONSTANT bool) = Right $ Constant bool
tokenToBoolExpr tokenIndex (IDENTIFIER string) = Right $ Variable string
tokenToBoolExpr tokenIndex token = Left $ "Unexpected token \"" ++ show token ++ "\" at position " ++ show (tokenIndex + 1) ++ ". Expected operand."

parseBoolExprWithRemainder :: Int -> BoolExpr -> [Token] -> Either String BoolExpr
parseBoolExprWithRemainder tokenIndex leftBoolExpr [] = Right leftBoolExpr
parseBoolExprWithRemainder tokenIndex leftBoolExpr remainder =
                                            if binaryOperator /= CONJUNCTION && binaryOperator /= DISJUNCTION
                                            then Left $ "Unexpected token \"" ++ show binaryOperator ++ "\" at position " ++ show (tokenIndex + 1) ++ ". Expected binary operator (\"and\", \"or\")."
                                            else do
                                                (rightBoolExpr, remainder, newTokenIndex) <- parseOperand (tokenIndex + 1) (tail remainder)
                                                case binaryOperator of
                                                    CONJUNCTION -> parseBoolExprWithRemainder newTokenIndex (Conjunction leftBoolExpr rightBoolExpr) remainder
                                                    DISJUNCTION -> parseBoolExprWithRemainder newTokenIndex (Disjunction leftBoolExpr rightBoolExpr) remainder
                                            where binaryOperator = head remainder

parseBracket :: Int -> [Token] -> Either String (BoolExpr, [Token], Int)
parseBracket tokenIndex tokens = case splitAtMatchingBracket tokens 1 of
                        Nothing -> Left $ "Expected closing bracket for opening bracket at position " ++ show tokenIndex ++ "."
                        Just (toMatchingBracket, remainder) -> do
                                                            boolExprBrackets <- parse tokenIndex toMatchingBracket
                                                            Right (boolExprBrackets, remainder, tokenIndex + length toMatchingBracket + 1)

parseOperand :: Int -> [Token] -> Either String (BoolExpr, [Token], Int)
parseOperand tokenIndex [] = Left $ "Expected operand after last operator at position " ++ show tokenIndex ++ "."
parseOperand tokenIndex [token] = do
                    boolExpr <- tokenToBoolExpr tokenIndex token
                    Right (boolExpr, [], tokenIndex + 1)
parseOperand tokenIndex (x:xs)
    | x == NEGATION = do
                    (boolExpr, remainder, newTokenIndex) <- parseOperand (tokenIndex + 1) xs
                    Right (Negation boolExpr, remainder, newTokenIndex)
    | isRight $ tokenToBoolExpr tokenIndex x = do
                                tokenBoolExpr <- tokenToBoolExpr tokenIndex x
                                Right (tokenBoolExpr, xs, tokenIndex + 1)
    | x == LBRACKET = parseBracket (tokenIndex + 1) xs
    | otherwise = Left $ "Unexpected token \"" ++ show x ++ "\" at position " ++ show (tokenIndex + 1) ++ ". Expected operand."

parse :: Int -> [Token] -> Either String BoolExpr
parse tokenIndex tokens = do
            (operand, remainder, newTokenIndex) <- parseOperand tokenIndex tokens
            parseBoolExprWithRemainder newTokenIndex operand remainder

toAbstractSyntaxTree :: String -> Either String BoolExpr
toAbstractSyntaxTree input = do
        tokens <- tokenize 0 [] input
        abstractSyntaxTree <- parse 0 tokens
        Right abstractSyntaxTree
