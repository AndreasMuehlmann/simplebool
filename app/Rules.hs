module Rules where

import qualified Parse as P

identity :: P.BoolExpr -> Maybe P.BoolExpr
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

complement :: P.BoolExpr -> Maybe P.BoolExpr
complement (P.Conjunction (P.Negation leftBoolExpr) rightBoolExpr)
  | leftBoolExpr == rightBoolExpr = Just $ P.Constant False
  | otherwise = Nothing
complement (P.Conjunction leftBoolExpr (P.Negation rightBoolExpr))
  | leftBoolExpr == rightBoolExpr = Just $ P.Constant False
  | otherwise = Nothing
complement (P.Disjunction (P.Negation leftBoolExpr) rightBoolExpr)
  | leftBoolExpr == rightBoolExpr = Just $ P.Constant True
  | otherwise = Nothing
complement (P.Disjunction leftBoolExpr (P.Negation rightBoolExpr))
  | leftBoolExpr == rightBoolExpr = Just $ P.Constant True
  | otherwise = Nothing
complement boolExpr = Nothing

absorption :: P.BoolExpr -> Maybe P.BoolExpr
absorption (P.Disjunction boolExprA (P.Conjunction boolExprB boolExprC))
  | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
  | otherwise = Nothing
absorption (P.Disjunction (P.Conjunction boolExprB boolExprC) boolExprA)
  | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
  | otherwise = Nothing
absorption (P.Conjunction boolExprA (P.Disjunction boolExprB boolExprC))
  | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
  | otherwise = Nothing
absorption (P.Conjunction (P.Disjunction boolExprB boolExprC) boolExprA)
  | boolExprA == boolExprB || boolExprA == boolExprC = Just boolExprA
  | otherwise = Nothing
absorption boolExpr = Nothing

kommutativity :: P.BoolExpr -> Maybe P.BoolExpr
kommutativity (P.Conjunction leftBoolExpr rightBoolExpr) = Just $ P.Conjunction rightBoolExpr leftBoolExpr
kommutativity (P.Disjunction leftBoolExpr rightBoolExpr) = Just $ P.Disjunction rightBoolExpr leftBoolExpr
kommutativity boolExpr = Nothing

assoziativity :: P.BoolExpr -> Maybe P.BoolExpr
assoziativity (P.Conjunction leftBoolExpr (P.Conjunction rightLeftBoolExpr rightRightBoolExpr)) = Just $ P.Conjunction (P.Conjunction leftBoolExpr rightLeftBoolExpr) rightRightBoolExpr
assoziativity (P.Conjunction (P.Conjunction leftLeftBoolExpr leftRightBoolExpr) rightBoolExpr) = Just $ P.Conjunction leftLeftBoolExpr (P.Conjunction leftRightBoolExpr rightBoolExpr)
assoziativity (P.Disjunction leftBoolExpr (P.Disjunction rightLeftBoolExpr rightRightBoolExpr)) = Just $ P.Disjunction (P.Disjunction leftBoolExpr rightLeftBoolExpr) rightRightBoolExpr
assoziativity (P.Disjunction (P.Disjunction leftLeftBoolExpr leftRightBoolExpr) rightBoolExpr) = Just $ P.Disjunction leftLeftBoolExpr (P.Disjunction leftRightBoolExpr rightBoolExpr)
assoziativity boolExpr = Nothing

distributivityExpanding :: P.BoolExpr -> Maybe P.BoolExpr
distributivityExpanding (P.Conjunction leftBoolExpr (P.Disjunction rightLeftBoolExpr rightRightBoolExpr)) = Just $ P.Disjunction (P.Conjunction leftBoolExpr rightLeftBoolExpr) (P.Conjunction leftBoolExpr rightRightBoolExpr)
distributivityExpanding (P.Disjunction leftBoolExpr (P.Conjunction rightLeftBoolExpr rightRightBoolExpr)) = Just $ P.Conjunction (P.Disjunction leftBoolExpr rightLeftBoolExpr) (P.Disjunction leftBoolExpr rightRightBoolExpr)
distributivityExpanding boolExpr = Nothing

distributivityFactoring :: P.BoolExpr -> Maybe P.BoolExpr
distributivityFactoring (P.Conjunction (P.Disjunction leftLeftBoolExpr leftRightBoolExpr) (P.Disjunction rightLeftBoolExpr rightRightBoolExpr))
  | leftLeftBoolExpr == rightLeftBoolExpr = Just $ P.Disjunction leftLeftBoolExpr (P.Conjunction leftRightBoolExpr rightRightBoolExpr)
  | leftLeftBoolExpr == rightRightBoolExpr = Just $ P.Disjunction leftLeftBoolExpr (P.Conjunction leftRightBoolExpr rightLeftBoolExpr)
  | leftRightBoolExpr == rightLeftBoolExpr = Just $ P.Disjunction leftRightBoolExpr (P.Conjunction leftLeftBoolExpr rightRightBoolExpr)
  | leftRightBoolExpr == rightRightBoolExpr = Just $ P.Disjunction leftRightBoolExpr (P.Conjunction leftLeftBoolExpr rightLeftBoolExpr)
  | otherwise = Nothing
distributivityFactoring (P.Disjunction (P.Conjunction leftLeftBoolExpr leftRightBoolExpr) (P.Conjunction rightLeftBoolExpr rightRightBoolExpr))
  | leftLeftBoolExpr == rightLeftBoolExpr = Just $ P.Conjunction leftLeftBoolExpr (P.Disjunction leftRightBoolExpr rightRightBoolExpr)
  | leftLeftBoolExpr == rightRightBoolExpr = Just $ P.Conjunction leftLeftBoolExpr (P.Disjunction leftRightBoolExpr rightLeftBoolExpr)
  | leftRightBoolExpr == rightLeftBoolExpr = Just $ P.Conjunction leftRightBoolExpr (P.Disjunction leftLeftBoolExpr rightRightBoolExpr)
  | leftRightBoolExpr == rightRightBoolExpr = Just $ P.Conjunction leftRightBoolExpr (P.Disjunction leftLeftBoolExpr rightLeftBoolExpr)
  | otherwise = Nothing
distributivityFactoring boolExpr = Nothing

demorgan :: P.BoolExpr -> Maybe P.BoolExpr 
demorgan (P.Negation (P.Conjunction leftBoolExpr rightBoolExpr)) = Just $ P.Disjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)
demorgan (P.Negation (P.Disjunction leftBoolExpr rightBoolExpr)) = Just $ P.Conjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)
demorgan (P.Conjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)) = Just $ P.Negation (P.Disjunction leftBoolExpr rightBoolExpr)
demorgan (P.Disjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)) = Just $ P.Negation (P.Conjunction leftBoolExpr rightBoolExpr)
demorgan boolExpr = Nothing
