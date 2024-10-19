module Main where

import qualified Parse as P
import qualified Simplify as S
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then print "Error: An argument with the boolean expression to be simplified is required."
    else do
      let result = P.toAbstractSyntaxTree $ head args
      case result of
        Right abstractSyntaxTree ->
          S.printSimplificationWithInitialExpr abstractSyntaxTree $
            S.simplify 0 abstractSyntaxTree
        Left err -> putStrLn $ "Error: " ++ err
