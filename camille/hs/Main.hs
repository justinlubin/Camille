module Main where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Error
import System.IO
import System.Environment

import Type
import Parser
import Environment
import TypeChecker
import Evaluator

stdlib :: String
stdlib = "../stdlib/stdlib.cam"

loadLibrary :: String -> Environment -> IO (Environment)
loadLibrary lib env = do
    contents <- readFile lib
    let program = "Integer {\n" ++  contents ++ "}"
    case (readExpression program) of
        Left err -> do
            print err
            return env
        Right val -> do
            typesCheck <- checkThrowsIO . stmToIO . checkType env $ val
            if (typesCheck)
                then do
                    retVal <- runThrowsIO VoidExpression . eval env $ val
                    return env
                else do
                    return env

runFile :: String -> IO (Expression)
runFile fileName = do
    contents <- readFile fileName
    let program = "Integer {\n" ++  contents ++ "}"
    env <- newEnvironmentIO >>= loadLibrary stdlib
    case (readExpression program) of
        Left err -> do
            print err
            return VoidExpression
        Right val -> do
            typesCheck <- checkThrowsIO . stmToIO . checkType env $ val
            if (typesCheck)
                then do
                    retVal <- runThrowsIO VoidExpression . eval env $ val
                    return retVal
                else do
                    return VoidExpression

repl :: Environment -> IO ()
repl env = do
    putStr "Camille> "
    hFlush stdout
    s <- getLine
    case (readExpression s) of
        Left err -> do
            print err
            repl env
        Right val -> do
            typesCheck <- checkThrowsIO . stmToIO . checkType env $ val
            when (typesCheck) $ do
                newVal <- runThrowsIO VoidExpression . eval env $ val
                when (newVal /= VoidExpression) $ do
                    print newVal
            repl env

main :: IO ()
main = do args <- getArgs
          if (length args == 0)
              then do
                  newEnvironmentIO >>= loadLibrary stdlib >>= repl
              else do
                  e <- runFile (args !! 0)
                  when (e /= VoidExpression) $ do
                      print e
