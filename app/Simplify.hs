{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Simplify where

import Control.Applicative
import Data.Foldable (maximumBy)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import qualified Parse as P
import qualified Rules as R

type RuleApplication = (P.BoolExpr, String)

type Simplification = [RuleApplication]


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
      [ (R.kommutativity, "kommutativity"),
        (R.assoziativity, "assoziativity"),
        (R.distributivityFactoring, "distributivity-factoring"),
        (R.distributivityExpanding, "distributivity-expanding"),
        (R.demorgan, "de-morgan")
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
      [ (R.annihilation, "annihilation"),
        (R.identity, "identity"),
        (R.duality, "duality"),
        (R.doubleNegation, "double-negation"),
        (R.idempotence, "idempotence"),
        (R.complement, "complement"),
        (R.absorption, "absorption")
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
