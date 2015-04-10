module Main where

import System.IO
import System.Environment
import Parser
import Evaluator

repl :: String -> String
repl s = case readExpression s of Left err  -> show err
                                  Right val -> show val

main :: IO ()
main = do putStr "Camille> "
          hFlush stdout
          s <- getLine
          putStrLn $ repl s
          main
