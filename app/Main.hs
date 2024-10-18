{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Main where

import System.Environment

import qualified Parse as P
import Parse (BoolExpr(Constant))


identity:: P.BoolExpr -> Maybe P.BoolExpr
identity (P.And (P.Constant True) rightBoolExpr) = Just rightBoolExpr
identity (P.And leftBoolExpr (P.Constant True)) = Just leftBoolExpr
identity (P.Or (P.Constant False) rightBoolExpr) = Just rightBoolExpr
identity (P.Or leftBoolExpr (P.Constant False)) = Just leftBoolExpr
identity boolExpr = Nothing

annihilation :: P.BoolExpr -> Maybe P.BoolExpr
annihilation (P.And (P.Constant False) rightBoolExpr) = Just $ P.Constant False
annihilation (P.And leftBoolExpr (P.Constant False)) = Just $ P.Constant False
annihilation (P.Or (P.Constant True) rightBoolExpr) = Just $ P.Constant True
annihilation (P.Or leftBoolExpr (P.Constant True)) = Just $ P.Constant True
annihilation boolExpr = Nothing

idempotence :: P.BoolExpr -> Maybe P.BoolExpr
idempotence (P.And leftBoolExpr rightBoolExpr)
    | leftBoolExpr == rightBoolExpr = Just leftBoolExpr
    | otherwise = Nothing
idempotence (P.Or leftBoolExpr rightBoolExpr)
    | leftBoolExpr == rightBoolExpr = Just leftBoolExpr
    | otherwise = Nothing
idempotence boolExpr = Nothing

applyRule :: (P.BoolExpr -> Maybe P.BoolExpr) -> String -> P.BoolExpr -> Maybe (P.BoolExpr, String)
applyRule rule ruleName boolExpr = case rule boolExpr of
                            Just newBoolExpr -> Just (newBoolExpr, ruleName)
                            Nothing -> case boolExpr of
                                P.And leftBoolExpr rightBoolExpr -> case applyRule rule ruleName leftBoolExpr of
                                    Just (newLeftBoolExpr, ruleName) -> Just (P.And newLeftBoolExpr rightBoolExpr, ruleName)
                                    Nothing -> case applyRule rule ruleName rightBoolExpr of
                                        Just (newRightBoolExpr, ruleName) -> Just (P.And leftBoolExpr newRightBoolExpr, ruleName)
                                        Nothing -> Nothing
                                P.Or leftBoolExpr rightBoolExpr -> case applyRule rule ruleName leftBoolExpr of
                                    Just (newLeftBoolExpr, ruleName) -> Just (P.Or newLeftBoolExpr rightBoolExpr, ruleName)
                                    Nothing -> case applyRule rule ruleName rightBoolExpr of
                                        Just (newRightBoolExpr, ruleName) -> Just (P.Or leftBoolExpr newRightBoolExpr, ruleName)
                                        Nothing -> Nothing
                                P.Negation boolExpr -> case applyRule rule ruleName boolExpr of
                                    Just (newNegatedBoolExpr, ruleName) -> Just (P.Negation newNegatedBoolExpr, ruleName)
                                    Nothing -> Nothing
                                _ -> Nothing

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
    Just val -> Just val
    Nothing  -> firstJust xs

applyRules :: P.BoolExpr -> Maybe (P.BoolExpr, String)
applyRules boolExpr = firstJust $ map ($ boolExpr) rules
                    where rules = [applyRule identity "identity", applyRule annihilation "annihilation", applyRule idempotence "idempotence"]

simplify :: P.BoolExpr -> [(P.BoolExpr, String)]
simplify boolExpr = case applyRules boolExpr of
                        Nothing -> []
                        Just (simplifiedBoolExpr, ruleName) -> (simplifiedBoolExpr, ruleName) : simplify simplifiedBoolExpr

printSimplification :: [(P.BoolExpr, String)] -> IO ()
printSimplification [] = return ()
printSimplification ((boolExpr, ruleName):xs) =  putStr (ruleName ++ " -> ") >> print boolExpr >> printSimplification xs

main :: IO ()
main = do
    args <- getArgs;
    if null args
    then print "Error: An argument with the boolean expression to be simplified is required."
    else do
        let result = P.toAbstractSyntaxTree $ head args
        case result of
            Right abstractSyntaxTree -> print abstractSyntaxTree
            Left err -> putStrLn $ "Error: " ++ err
