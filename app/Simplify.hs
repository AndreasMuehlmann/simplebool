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

negation :: P.BoolExpr -> Maybe P.BoolExpr
negation (P.Negation (P.Constant val)) = Just $ P.Constant (not val)
negation boolExpr = Nothing

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

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
    Just val -> Just val
    Nothing  -> firstJust xs

applyRules :: P.BoolExpr -> Maybe (P.BoolExpr, String)
applyRules boolExpr = firstJust $ map ($ boolExpr) rules
                    where rules = [ applyRule annihilation "annihilation"
                                  , applyRule identity "identity"
                                  , applyRule negation "negation"
                                  , applyRule idempotence "idempotence"
                                  ]

simplify :: P.BoolExpr -> [(P.BoolExpr, String)]
simplify boolExpr = case applyRules boolExpr of
                        Nothing -> []
                        Just (simplifiedBoolExpr, ruleName) -> (simplifiedBoolExpr, ruleName) : simplify simplifiedBoolExpr

printSimplification :: [(P.BoolExpr, String)] -> IO ()
printSimplification [] = return ()
printSimplification ((boolExpr, ruleName):xs) =  putStr (ruleName ++ " -> ") >> print boolExpr >> printSimplification xs
