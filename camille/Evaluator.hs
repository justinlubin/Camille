module Evaluator where

import Control.Concurrent.STM
import Control.Monad

import Type
import Environment

eval :: Environment -> Expression -> IO (Expression)
eval env NothingExpression = return NothingExpression
eval env (BlockExpression t b) = do
    newEnv <- atomically $ newScope env [] []
    foldM (foldEval newEnv) NothingExpression b
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
eval env val@(TypeDeclarationExpression i t) = do atomically $ setType env i t
                                                  return NothingExpression
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
