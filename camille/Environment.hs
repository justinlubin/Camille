module Environment where

import Control.Concurrent.STM
import Control.Monad

import Parser

type Environment = TVar ( [(Identifier, TVar Type)]
                        , [(Identifier, TVar Expression)]
                        )

newEnvironment :: STM (Environment)
newEnvironment = newTVar ([], [])

newEnvironmentIO :: IO (Environment)
newEnvironmentIO = newTVarIO ([], [])

showEnv :: Environment -> STM (String)
showEnv env = do
    (_, varList) <- readTVar env
    liftM concat $ forM varList $ \(i, et) -> do
                       e <- readTVar et
                       t <- getType env i
                       return $    i
                                ++ ": "
                                ++ (show e)
                                ++ " ("
                                ++ (show t)
                                ++ ")\n"

setVariable :: Environment -> Identifier -> Expression -> STM ()
setVariable env i e = do (typeList, varList) <- readTVar env
                         case (lookup i varList) of
                             Nothing -> do et <- newTVar e
                                           writeTVar env ( typeList
                                                         , (i, et) : varList
                                                         )
                             Just et -> writeTVar et e

getVariable :: Environment -> Identifier -> STM (Expression)
getVariable env i = do (typeList, varList) <- readTVar env
                       case (lookup i varList) of
                           Nothing -> error $ "[TODO] ERROR! Variable not found: "
                                              ++ (show i)
                           Just t  -> readTVar t

setType :: Environment -> Identifier -> Type -> STM ()
setType env i t = do (typeList, varList) <- readTVar env
                     case (lookup i typeList) of
                         Nothing -> do tt <- newTVar t
                                       writeTVar env ( (i, tt) : typeList
                                                     , varList
                                                     )
                         Just tt -> writeTVar tt t

getType :: Environment -> Identifier -> STM (Type)
getType env i = do (typeList, _) <- readTVar env
                   case (lookup i typeList) of
                       Nothing -> error $ "[TODO] ERROR! Type not found: " ++ (show i)
                       Just t  -> readTVar t

