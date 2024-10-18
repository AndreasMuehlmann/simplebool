module Main where

import System.Environment

import qualified Parse as P
import qualified Simplify as S

main :: IO ()
main = do
    args <- getArgs;
    if null args
    then print "Error: An argument with the boolean expression to be simplified is required."
    else do
        let result = P.toAbstractSyntaxTree $ head args
        case result of
            Right abstractSyntaxTree -> S.printSimplificationWithInitialExpr abstractSyntaxTree $
                                        S.simplify abstractSyntaxTree
            Left err -> putStrLn $ "Error: " ++ err
