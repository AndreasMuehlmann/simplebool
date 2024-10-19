applyRule :: (P.BoolExpr -> Maybe P.BoolExpr) -> P.BoolExpr -> Maybe P.BoolExpr
applyRule rule boolExpr =
  rule boolExpr
    <|> case boolExpr of
      P.Conjunction leftBoolExpr rightBoolExpr -> ((`P.Conjunction` rightBoolExpr) <$> applyRule rule leftBoolExpr) <|> (P.Conjunction leftBoolExpr <$> applyRule rule rightBoolExpr)
      P.Disjunction leftBoolExpr rightBoolExpr -> ((`P.Disjunction` rightBoolExpr) <$> applyRule rule leftBoolExpr) <|> (P.Disjunction leftBoolExpr <$> applyRule rule rightBoolExpr)
      P.Negation boolExpr -> P.Negation <$> applyRule rule boolExpr
      _ -> Nothing

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
