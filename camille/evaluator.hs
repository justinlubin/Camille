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

showEnv :: Environment -> STM (String)
showEnv env = do envList <- readTVar env
                 liftM concat $ forM envList $ \x -> do
                                    let i = fst x
                                    e <- readTVar (snd x)
                                    return $ i ++ ": " ++ (show e) ++ "   ;   "

setVariable :: Environment -> Identifier -> Expression -> STM ()
setVariable env i e = do envList <- readTVar env
                         case (lookup i envList) of
                             Nothing -> do et <- newTVar e
                                           writeTVar env ((i, et) : envList)
                             Just et -> writeTVar et e

getVariable :: Environment -> Identifier -> STM (Expression)
getVariable env i = do envList <- readTVar env
                       case (lookup i envList) of Nothing -> return NothingExpression
                                                  Just t  -> readTVar t

newScope :: Environment -> [Identifier] -> [Expression] -> STM (Environment)
newScope oldEnv is es = do
    envList <- readTVar oldEnv
    newEnv <- newEnvironment
    forM_ envList $ \x -> do
        let i = fst x
        e <- readTVar (snd x)
        setVariable newEnv i e
    zipWithM_ (setVariable newEnv) is es
    return newEnv

eval :: Environment -> Expression -> IO (Expression)
eval env NothingExpression = return NothingExpression
eval env (BlockExpression body) = do
    newEnv <- atomically $ newScope env [] []
    foldM (foldEval newEnv) NothingExpression body
  where
    foldEval blockEnv result expr = do
    if (result /= NothingExpression)
        then do
            return result
        else do
            r <- eval blockEnv expr
            case r of
                RetExpression e -> eval blockEnv e
                otherwise       -> return result
eval env val@(IntegerExpression _) = return val
eval env val@(StringExpression _) = return val
eval env val@(BooleanExpression _) = return val
eval env (IfExpression condition truePath falsePath) = do
    (BooleanExpression success) <- eval env condition
    let path = if success then truePath else falsePath
    eval env path
eval env val@(LambdaExpression params expressions) = return val
eval env val@(RetExpression _) = return val
eval env val@(TypeDeclarationExpression _ _) = return val
eval env (FCallExpression "neg" [e]) =
    eval env e >>= return . negInteger
eval env (FCallExpression "pred" [e]) =
    eval env e >>= return . predInteger
eval env (FCallExpression "succ" [e]) =
    eval env e >>= return . succInteger
eval env (FCallExpression "add" es) =
    mapM (eval env) es >>= return . foldInteger (+)
eval env (FCallExpression "mul" es) =
    mapM (eval env) es >>= return . foldInteger (*)
eval env (FCallExpression "eq" es) =
    mapM (eval env) es >>= return . allExpression (==)
eval env (FCallExpression "lt" es) =
    mapM (eval env) es >>= return . allExpression (<)
eval env (FCallExpression "lte" es) =
    mapM (eval env) es >>= return . allExpression (<=)
eval env (FCallExpression "gt" es) =
    mapM (eval env) es >>= return . allExpression (>)
eval env (FCallExpression "gte" es) =
    mapM (eval env) es >>= return . allExpression (>=)
eval env (FCallExpression "print" [e]) =
    eval env e >>= print >> return NothingExpression
eval env (FCallExpression "env" []) =
    (atomically . showEnv) env >>= return . StringExpression
eval env (FCallExpression name args) = do
    f@(LambdaExpression params body) <- atomically $ getVariable env name
    evaluatedArgs <- mapM (eval env) args
    newEnv <- atomically $ newScope env params evaluatedArgs
    eval newEnv body
eval env (AssignmentExpression i e) = do newE <- eval env e
                                         atomically $ setVariable env i newE
                                         return newE
eval env (VariableExpression i) = atomically $ getVariable env i

-- Builtins

negInteger :: Expression -> Expression
negInteger (IntegerExpression n) = IntegerExpression (-n)

predInteger :: Expression -> Expression
predInteger (IntegerExpression n) = IntegerExpression (n - 1)

succInteger :: Expression -> Expression
succInteger (IntegerExpression n) = IntegerExpression (n + 1)

foldInteger :: (Integer -> Integer -> Integer) -> [Expression] -> Expression
foldInteger f = foldl1 $ \(IntegerExpression a) (IntegerExpression n) ->
                             IntegerExpression (f a n)

allExpression :: (Expression -> Expression -> Bool) ->
                 [Expression] -> Expression
allExpression f (e:es) = BooleanExpression $ all (\x -> f e x) es
