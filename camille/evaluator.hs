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
                       case (lookup i envList) of Nothing -> return ENothing
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

eval :: Environment -> Expression -> STM (Expression)
eval env ENothing = return ENothing
eval env (EBlock body) = do
    newEnv <- newScope env [] []
    foldM (foldEval newEnv) ENothing body
  where
    foldEval blockEnv result expr = do
    if (result /= ENothing)
        then do
            return result
        else do
            r <- eval blockEnv expr
            case r of
                ERet e    -> eval blockEnv e
                otherwise -> return result
eval env val@(EInteger _) = return val
eval env val@(EString _) = return val
eval env val@(EBoolean _) = return val
eval env (EIf condition truePath falsePath) = do
    (EBoolean success) <- eval env condition
    let path = if success then truePath else falsePath
    eval env path
eval env val@(ELambda params expressions) = return val
eval env val@(ERet _) = return val
eval env val@(ETypeDeclaration _ _) = return val
eval env (EFCall "neg" [e]) = eval env e >>= return . negInteger
eval env (EFCall "pred" [e]) = eval env e >>= return . predInteger
eval env (EFCall "succ" [e]) = eval env e >>= return . succInteger
eval env (EFCall "add" es) = mapM (eval env) es >>= return . addIntegers
eval env (EFCall "mul" es) = mapM (eval env) es >>= return . multiplyIntegers
eval env (EFCall "eq" es) = mapM (eval env) es >>= return . allEqual
eval env (EFCall "env" []) = showEnv env >>= return . EString
eval env (EFCall name args) = do
    f@(ELambda params body) <- getVariable env name
    evaluatedArgs <- mapM (eval env) args
    newEnv <- newScope env params evaluatedArgs
    eval newEnv body
eval env (EAssignment i e) = do newE <- eval env e
                                setVariable env i newE 
                                return newE
eval env (EVariable i) = getVariable env i

-- Builtins

negInteger :: Expression -> Expression
negInteger (EInteger n) = EInteger (-n)

predInteger :: Expression -> Expression
predInteger (EInteger n) = EInteger (n - 1)

succInteger :: Expression -> Expression
succInteger (EInteger n) = EInteger (n + 1)

addIntegers :: [Expression] -> Expression
addIntegers = foldl1 (\(EInteger a) (EInteger n) -> EInteger (a + n))

multiplyIntegers :: [Expression] -> Expression
multiplyIntegers = foldl1 (\(EInteger a) (EInteger n) -> EInteger (a * n))

allEqual :: [Expression] -> Expression
allEqual (e:es) = EBoolean $ all (== e) es
