{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Simplify where

import Control.Applicative
import Data.Foldable (maximumBy)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import qualified Parse as P
import qualified Type.Reflection as P
import Parse (BoolExpr(Negation))

type RuleApplication = (P.BoolExpr, String)

type Simplification = [RuleApplication]

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
demorgan (Negation (P.Conjunction leftBoolExpr rightBoolExpr)) = Just $ P.Conjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)
demorgan (Negation (P.Disjunction leftBoolExpr rightBoolExpr)) = Just $ P.Disjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)
demorgan (P.Conjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)) = Just $ P.Negation (P.Conjunction leftBoolExpr rightBoolExpr)
demorgan (P.Disjunction (P.Negation leftBoolExpr) (P.Negation rightBoolExpr)) = Just $ P.Negation (P.Disjunction leftBoolExpr rightBoolExpr)
demorgan boolExpr = Nothing

applyRule :: (P.BoolExpr -> Maybe P.BoolExpr) -> P.BoolExpr -> Maybe P.BoolExpr
applyRule rule boolExpr =
  rule boolExpr
    <|> case boolExpr of
      P.Conjunction leftBoolExpr rightBoolExpr -> ((`P.Conjunction` rightBoolExpr) <$> applyRule rule leftBoolExpr) <|> (P.Conjunction leftBoolExpr <$> applyRule rule rightBoolExpr)
      P.Disjunction leftBoolExpr rightBoolExpr -> ((`P.Disjunction` rightBoolExpr) <$> applyRule rule leftBoolExpr) <|> (P.Disjunction leftBoolExpr <$> applyRule rule rightBoolExpr)
      P.Negation boolExpr -> P.Negation <$> applyRule rule boolExpr
      _ -> Nothing

allRuleApplications :: (P.BoolExpr -> Maybe P.BoolExpr) -> P.BoolExpr -> [P.BoolExpr]
allRuleApplications rule boolExpr =
  maybeToList (rule boolExpr)
    ++ case boolExpr of
      P.Conjunction leftBoolExpr rightBoolExpr -> map (`P.Conjunction` rightBoolExpr) (allRuleApplications rule leftBoolExpr) ++ map (P.Conjunction leftBoolExpr) (allRuleApplications rule rightBoolExpr)
      P.Disjunction leftBoolExpr rightBoolExpr -> map (`P.Disjunction` rightBoolExpr) (allRuleApplications rule leftBoolExpr) ++ map (P.Disjunction leftBoolExpr) (allRuleApplications rule rightBoolExpr)
      P.Negation boolExpr -> map P.Negation (allRuleApplications rule boolExpr)
      _ -> []

applyRules :: [(P.BoolExpr -> Maybe P.BoolExpr, String)] -> P.BoolExpr -> Maybe RuleApplication
applyRules rules boolExpr = foldr (<|>) Nothing appliedRules
  where
    curried = map (\(rule, ruleName) -> (applyRule rule, ruleName)) rules
    appliedRules = map (\(ruleApplication, ruleName) -> (\newBoolExpr -> (newBoolExpr, ruleName)) <$> ruleApplication boolExpr) curried

allRuleApplicationsForRules :: [(P.BoolExpr -> Maybe P.BoolExpr, String)] -> P.BoolExpr -> [RuleApplication]
allRuleApplicationsForRules rules boolExpr = concat appliedRules
  where
    curried = map (\(rule, ruleName) -> (allRuleApplications rule, ruleName)) rules
    appliedRules = map (\(ruleApplication, ruleName) -> (\newBoolExpr -> (newBoolExpr, ruleName)) <$> ruleApplication boolExpr) curried

allRuleApplicationsChangingRules :: P.BoolExpr -> [RuleApplication]
allRuleApplicationsChangingRules = allRuleApplicationsForRules changingRules
  where
    changingRules =
      [ (kommutativity, "kommutativity"),
        (assoziativity, "assoziativity"),
        (distributivityFactoring, "distributivity-factoring"),
        (distributivityExpanding, "distributivity-expanding"),
        (demorgan, "de-morgan")
      ]

complexity :: P.BoolExpr -> Int
complexity (P.Conjunction leftBoolExpr rightBoolExpr) = 1 + complexity leftBoolExpr + complexity rightBoolExpr
complexity (P.Disjunction leftBoolExpr rightBoolExpr) = 1 + complexity leftBoolExpr + complexity rightBoolExpr
complexity (P.Negation boolExpr) = 1 + complexity boolExpr
complexity (P.Constant value) = 1
complexity (P.Variable name) = 1

applySimplifyingRules :: P.BoolExpr -> Simplification
applySimplifyingRules boolExpr = case applyRules simplifyingRules boolExpr of
  Nothing -> []
  Just (simplifiedBoolExpr, ruleName) -> (simplifiedBoolExpr, ruleName) : applySimplifyingRules simplifiedBoolExpr
  where
    simplifyingRules =
      [ (annihilation, "annihilation"),
        (identity, "identity"),
        (duality, "duality"),
        (doubleNegation, "double-negation"),
        (idempotence, "idempotence"),
        (complement, "complement"),
        (absorption, "absorption")
      ]

maxDepth :: Int
maxDepth = 5

complexitySimplification :: Int -> Simplification -> Int
complexitySimplification baseComplexity [] = baseComplexity
complexitySimplification baseComplexity simplification = complexity $ fst $ last simplification

compareSimplifications :: Int -> Simplification -> Simplification -> Ordering
compareSimplifications baseComplexity leftSimplification rightSimplification
  | complexityLeft < complexityRight = GT
  | complexityLeft > complexityRight = LT
  | complexityLeft == complexityRight = if lengthLeft > lengthRight then LT else (if lengthLeft < lengthRight then GT else EQ)
  where
    complexityLeft = complexitySimplification baseComplexity leftSimplification
    complexityRight = complexitySimplification baseComplexity rightSimplification
    lengthLeft = length leftSimplification
    lengthRight = length rightSimplification

bestSimplification :: Int -> [Simplification] -> Simplification
bestSimplification baseComplexity simplifications = maximumBy (compareSimplifications baseComplexity) ([] : simplifications)

simplify :: Int -> P.BoolExpr -> [RuleApplication]
simplify depth boolExpr
  | depth >= maxDepth = simplifyingRulesApplied
  | otherwise = simplifyingRulesApplied ++ bestSimplification (complexity boolExprToSimplify) simplifications
  where
    simplifyingRulesApplied = applySimplifyingRules boolExpr
    boolExprToSimplify = case simplifyingRulesApplied of
      [] -> boolExpr
      [x] -> fst x
      _ -> fst $ last simplifyingRulesApplied
    simplifications = map (\(boolExpr, ruleName) -> (boolExpr, ruleName) : simplify (depth + 1) boolExpr) (allRuleApplicationsChangingRules boolExprToSimplify)

printSimplificationWithInitialExpr :: P.BoolExpr -> Simplification -> IO ()
printSimplificationWithInitialExpr initialBoolExpr simplification = print initialBoolExpr >> printSimplification simplification

printSimplification :: Simplification -> IO ()
printSimplification [] = return ()
printSimplification ((boolExpr, ruleName) : xs) = putStr (ruleName ++ " -> ") >> print boolExpr >> printSimplification xs
