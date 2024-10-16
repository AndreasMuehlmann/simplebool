{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Simplify where

import qualified Parse as P


identity:: P.BoolExpr -> Maybe P.BoolExpr
identity (P.Conjunction (P.Constant True) rightBoolExpr) = Just rightBoolExpr
identity (P.Conjunction leftBoolExpr (P.Constant True)) = Just leftBoolExpr
identity (P.Disjunction (P.Constant False) rightBoolExpr) = Just rightBoolExpr
identity (P.Disjunction leftBoolExpr (P.Constant False)) = Just leftBoolExpr
identity boolExpr = Nothing

annihilation :: P.BoolExpr -> Maybe P.BoolExpr
annihilation (P.Conjunction (P.Constant False) rightBoolExpr) = Just $ P.Constant False
annihilation (P.Conjunction leftBoolExpr (P.Constant False)) = Just $ P.Constant False
annihilation (P.Disjunction (P.Constant True) rightBoolExpr) = Just $ P.Constant True
annihilation (P.Disjunction leftBoolExpr (P.Constant True)) = Just $ P.Constant True
annihilation boolExpr = Nothing

idempotence :: P.BoolExpr -> Maybe P.BoolExpr
idempotence (P.Conjunction leftBoolExpr rightBoolExpr)
    | leftBoolExpr == rightBoolExpr = Just leftBoolExpr
    | otherwise = Nothing
idempotence (P.Disjunction leftBoolExpr rightBoolExpr)
    | leftBoolExpr == rightBoolExpr = Just leftBoolExpr
    | otherwise = Nothing
idempotence boolExpr = Nothing

duality :: P.BoolExpr -> Maybe P.BoolExpr
duality (P.Negation (P.Constant val)) = Just $ P.Constant (not val)
duality boolExpr = Nothing

doubleNegation :: P.BoolExpr -> Maybe P.BoolExpr
doubleNegation (P.Negation (P.Negation boolExpr)) = Just boolExpr
doubleNegation boolExpr = Nothing

komplement :: P.BoolExpr -> Maybe P.BoolExpr
komplement (P.Conjunction (P.Negation leftBoolExpr) rightBoolExpr)
    | leftBoolExpr == rightBoolExpr = Just $ P.Constant False
    | otherwise = Nothing
komplement (P.Conjunction leftBoolExpr (P.Negation rightBoolExpr))
    | leftBoolExpr == rightBoolExpr = Just $ P.Constant False
    | otherwise = Nothing
komplement (P.Disjunction (P.Negation leftBoolExpr) rightBoolExpr)
    | leftBoolExpr == rightBoolExpr = Just $ P.Constant True
    | otherwise = Nothing
komplement (P.Disjunction leftBoolExpr (P.Negation rightBoolExpr))
    | leftBoolExpr == rightBoolExpr = Just $ P.Constant True
    | otherwise = Nothing
komplement boolExpr = Nothing

absorption :: P.BoolExpr -> Maybe P.BoolExpr
absorption (P.Disjunction boolExprA (P.Conjunction boolExprB boolExprC))
    | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
absorption (P.Disjunction (P.Conjunction boolExprB boolExprC) boolExprA)
    | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
absorption (P.Conjunction boolExprA (P.Disjunction boolExprB boolExprC))
    | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
absorption (P.Conjunction (P.Disjunction boolExprB boolExprC) boolExprA)
    | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
absorption boolExpr = Nothing

applyRule :: (P.BoolExpr -> Maybe P.BoolExpr) -> String -> P.BoolExpr -> Maybe (P.BoolExpr, String)
applyRule rule ruleName boolExpr = case rule boolExpr of
                            Just newBoolExpr -> Just (newBoolExpr, ruleName)
                            Nothing -> case boolExpr of
                                P.Conjunction leftBoolExpr rightBoolExpr -> case applyRule rule ruleName leftBoolExpr of
                                    Just (newLeftBoolExpr, ruleName) -> Just (P.Conjunction newLeftBoolExpr rightBoolExpr, ruleName)
                                    Nothing -> case applyRule rule ruleName rightBoolExpr of
                                        Just (newRightBoolExpr, ruleName) -> Just (P.Conjunction leftBoolExpr newRightBoolExpr, ruleName)
                                        Nothing -> Nothing
                                P.Disjunction leftBoolExpr rightBoolExpr -> case applyRule rule ruleName leftBoolExpr of
                                    Just (newLeftBoolExpr, ruleName) -> Just (P.Disjunction newLeftBoolExpr rightBoolExpr, ruleName)
                                    Nothing -> case applyRule rule ruleName rightBoolExpr of
                                        Just (newRightBoolExpr, ruleName) -> Just (P.Disjunction leftBoolExpr newRightBoolExpr, ruleName)
                                        Nothing -> Nothing
                                P.Negation boolExpr -> case applyRule rule ruleName boolExpr of
                                    Just (newNegatedBoolExpr, ruleName) -> Just (P.Negation newNegatedBoolExpr, ruleName)
                                    Nothing -> Nothing
                                _ -> Nothing

allRuleApplications :: (P.BoolExpr -> Maybe P.BoolExpr) -> String -> P.BoolExpr -> [(P.BoolExpr, String)]
allRuleApplications rule ruleName boolExpr = case rule boolExpr of
                                                Nothing -> case boolExpr of
                                                    P.Conjunction leftBoolExpr rightBoolExpr -> map (\(newBoolExpr, ruleName) -> (P.Conjunction newBoolExpr rightBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr) ++ map (\(newBoolExpr, ruleName) -> (P.Conjunction leftBoolExpr newBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr)
                                                    P.Disjunction leftBoolExpr rightBoolExpr -> map (\(newBoolExpr, ruleName) -> (P.Disjunction newBoolExpr rightBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr) ++ map (\(newBoolExpr, ruleName) -> (P.Disjunction leftBoolExpr newBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr)
                                                    P.Negation boolExpr -> map (\(newBoolExpr, ruleName) -> (P.Negation newBoolExpr, ruleName)) (allRuleApplications rule ruleName boolExpr)
                                                    _ -> []
                                                Just newBoolExpr -> (newBoolExpr, ruleName) : (case boolExpr of
                                                    P.Conjunction leftBoolExpr rightBoolExpr -> map (\(newBoolExpr, ruleName) -> (P.Conjunction newBoolExpr rightBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr) ++ map (\(newBoolExpr, ruleName) -> (P.Conjunction leftBoolExpr newBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr)
                                                    P.Disjunction leftBoolExpr rightBoolExpr -> map (\(newBoolExpr, ruleName) -> (P.Disjunction newBoolExpr rightBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr) ++ map (\(newBoolExpr, ruleName) -> (P.Disjunction leftBoolExpr newBoolExpr, ruleName)) (allRuleApplications rule ruleName leftBoolExpr)
                                                    P.Negation boolExpr -> map (\(newBoolExpr, ruleName) -> (P.Negation newBoolExpr, ruleName)) (allRuleApplications rule ruleName boolExpr)
                                                    _ -> [])

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
    Just val -> Just val
    Nothing  -> firstJust xs

applyRules :: [(P.BoolExpr -> Maybe P.BoolExpr, String)] -> P.BoolExpr -> Maybe (P.BoolExpr, String)
applyRules rules boolExpr = firstJust $ map ($ boolExpr) curried
                    where curried = map (uncurry applyRule) rules

score :: P.BoolExpr -> Int
score (P.Conjunction leftBoolExpr rightBoolExpr) = 1 + score leftBoolExpr + score rightBoolExpr
score (P.Disjunction leftBoolExpr rightBoolExpr) = 1 + score leftBoolExpr + score rightBoolExpr
score (P.Negation boolExpr) = 1 + score boolExpr
score (P.Constant value) = 1
score (P.Variable name) = 1

applySimplifyingRules :: P.BoolExpr -> [(P.BoolExpr, String)]
applySimplifyingRules boolExpr = case applyRules simplifyingRules boolExpr of
                        Nothing -> []
                        Just (simplifiedBoolExpr, ruleName) -> (simplifiedBoolExpr, ruleName) : applySimplifyingRules simplifiedBoolExpr
                  where simplifyingRules = [ (annihilation, "annihilation")
                                           , (identity, "identity")
                                           , (duality, "duality")
                                           , (doubleNegation, "double negation")
                                           , (idempotence, "idempotence")
                                           , (komplement, "komplement")
                                           , (absorption, "absorption")
                                           ]

printSimplificationWithInitialExpr :: P.BoolExpr -> [(P.BoolExpr, String)] -> IO ()
printSimplificationWithInitialExpr initialBoolExpr simplification = print initialBoolExpr >> printSimplification simplification

printSimplification :: [(P.BoolExpr, String)] -> IO ()
printSimplification [] = return ()
printSimplification ((boolExpr, ruleName):xs) =  putStr (ruleName ++ " -> ") >> print boolExpr >> printSimplification xs
