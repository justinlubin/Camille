module Environment where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Error

import Type

type Environment = TVar ( [(Identifier, TVar Type)]
                        , [(Identifier, TVar Expression)]
                        )

newEnvironment :: STM (Environment)
newEnvironment = newTVar ([], [])

newEnvironmentIO :: IO (Environment)
newEnvironmentIO = newTVarIO ([], [])

showEnv :: Environment -> STMThrowsError (String)
showEnv env = do
    (_, varList) <- (lift . readTVar) env
    liftM concat $ forM varList $ \(i, et) -> do
                       e <- (lift . readTVar) et
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

getVariable :: Environment -> Identifier -> STMThrowsError (Expression)
getVariable env i = do (typeList, varList) <- lift . readTVar $ env
                       case (lookup i varList) of
                           Nothing -> throwError $ NoSuchVariableError i
                           Just t  -> lift . readTVar $ t

setType :: Environment -> Identifier -> Type -> STMThrowsError ()
setType env i t = do (typeList, varList) <- lift $ readTVar env
                     case (lookup i typeList) of
                         Nothing -> do tt <- lift $ newTVar t
                                       lift $ writeTVar env
                                                        ( (i, tt) : typeList
                                                        , varList
                                                        )
                         Just tt -> throwError $ TypeDeclarationAlreadyExistsError i

getType :: Environment -> Identifier -> STMThrowsError (Type)
getType env i = do (typeList, _) <- (lift . readTVar) env
                   case (lookup i typeList) of
                       Nothing -> throwError $ TypeDeclarationNotFoundError i
                       Just t  -> (lift . readTVar) t

newScope :: Environment -> [TypedIdentifier] -> Maybe [Expression] -> STMThrowsError (Environment)
newScope oldEnv typedIdentifiers mes = do
    (typeList, varList) <- lift $ readTVar oldEnv
    newEnv <- lift $ newEnvironment
    forM_ typeList $ \(i, tt) -> do
        t <- lift $ readTVar tt
        setType newEnv i t
    forM_ varList $ \(i, et) -> do
        e <- lift $ readTVar et
        lift $ setVariable newEnv i e
    case mes of
        Just es -> zipWithM_ (setup newEnv) typedIdentifiers es
        Nothing -> forM_ typedIdentifiers (setupTypes newEnv)
    return newEnv
  where
    setup env (TypedIdentifier i t) e = do setType env i t
                                           lift $ setVariable env i e
    setupTypes env (TypedIdentifier i t) = setType env i t
