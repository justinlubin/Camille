module Main where

import Control.Concurrent.STM
import System.IO
import System.Environment
import Parser
import Evaluator

repl :: Environment -> IO ()
repl env = do putStr "Camille> "
              hFlush stdout
              s <- getLine
              case (readExpression s) of
                  Left err -> do
                      print err
                      repl env
                  Right val -> do
                      newVal <- atomically . eval env $ val
                      print newVal
                      repl env

main :: IO ()
main = newEnvironmentIO >>= repl
