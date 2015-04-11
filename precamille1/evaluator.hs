module Evaluator where

import Control.Concurrent.STM
import Control.Monad
import System.Environment
import Parser

type Environment = TVar [(Identifier, TVar Expression)]

newEnvironment :: STM (Environment)
newEnvironment = newTVar [] 

newEnvironmentIO :: IO (Environment)
newEnvironmentIO = newTVarIO []

addVariable :: Environment -> Identifier -> Expression -> STM ()
addVariable env i e = do envList <- readTVar env
                         et <- newTVar e
                         writeTVar env ((i, et) : envList)

getVariable :: Environment -> Identifier -> STM (Expression)
getVariable env i = do envList <- readTVar env
                       case (lookup i envList) of Nothing -> retry
                                                  Just t  -> readTVar t

eval :: Environment -> Expression -> STM (Expression)
eval env None = return None
eval env val@(Integer _) = return val
eval env val@(String _) = return val
eval env val@(Boolean _) = return val
eval env val@(Lambda params expressions) = foldM foldEval None expressions
    where foldEval result expression = do
              if (result /= None)
                  then return result
                  else do r <- eval env expression
                          case r of Ret e     -> eval env e
                                    otherwise -> return result
eval env val@(Ret _) = return val
eval env val@(TypeDeclaration _ _) = return val
eval env val@(FCall "neg" [e]) = eval env e >>= return . negInteger
eval env val@(FCall "add" es) = mapM (eval env) es >>= return . addIntegers
eval env val@(FCall name args) = return val
eval env (Assignment i e) = do newE <- eval env e
                               addVariable env i newE 
                               return newE
eval env (Variable i) = getVariable env i

-- Builtins

negInteger :: Expression -> Expression
negInteger (Integer n) = Integer (-n)

addIntegers :: [Expression] -> Expression
addIntegers = foldl1 (\(Integer a) (Integer n) -> Integer (a + n))
