module Main where

import Control.Concurrent.STM
import Control.Monad
import System.IO
import System.Environment

import Type
import Parser
import Environment
import TypeChecker
import Evaluator

runFile :: String -> IO (Expression)
runFile fileName = do contents <- readFile fileName
                      let program = "Integer {\n" ++ contents ++ "}"
                      env <- newEnvironmentIO
                      case (readExpression program) of
                          Left err -> do
                              print err
                              return VoidExpression
                          Right val -> do
                              retVal <- eval env val
                              return retVal

repl :: Environment -> IO ()
repl env = do putStr "Camille> "
              hFlush stdout
              s <- getLine
              case (readExpression s) of
                  Left err -> do
                      print err
                      repl env
                  Right val -> do
                      newVal <- eval env $ val
                      when (newVal /= VoidExpression) $ do
                          print newVal
                      repl env

main :: IO ()
main = do args <- getArgs
          if (length args == 0)
              then do
                  newEnvironmentIO >>= repl
              else do
                  e <- runFile (args !! 0)
                  when (e /= VoidExpression) $ do
                    print e
