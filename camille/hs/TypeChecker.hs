module TypeChecker where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Error

import Type
import Environment

resolveType :: Environment -> Expression -> STMThrowsError (Type)
resolveType env VoidExpression = return $ CallableType [] VoidType
resolveType env (BlockExpression t _) = return t
resolveType env (IntegerExpression _) = return $ CallableType [] IntegerType
resolveType env (StringExpression _) = return $ CallableType [] StringType
resolveType env (BooleanExpression _) = return $ CallableType [] BooleanType
resolveType env (IfExpression _ a _) = resolveType env a
resolveType env (LambdaExpression params e) = do
    newEnv <- newScope env params Nothing
    returnType <- resolveType newEnv e
    return $ CallableType (map typedIdentifierType params)
                          returnType
  where
    typedIdentifierType (TypedIdentifier i t) = t
resolveType env (RetExpression e) = resolveType env e
resolveType env (TypeDeclarationExpression _) = return VoidType
resolveType env (FCallExpression i _) = getType env i >>= return . returnType
resolveType env (AssignmentExpression i e) = getType env i
resolveType env (VariableExpression i) = getType env i

checkType :: Environment -> Expression -> STMThrowsError ()
checkType env VoidExpression = return ()
checkType env (BlockExpression t es) = foldM_ foldCheck () es
  where
    foldCheck result e = do
        case e of
            (RetExpression _) -> do
                actual <- resolveType env e
                if (actual == t)
                    then return ()
                    else throwError $ TypeMismatchError actual t e
            _ -> return ()
        checkType env e
checkType env (IntegerExpression _) = return ()
checkType env (StringExpression _) = return ()
checkType env (BooleanExpression _) = return ()
checkType env (IfExpression c a b) = do
    checkType env a
    checkType env b
    conditionType <- resolveType env c
    pathAType <- resolveType env a
    pathBType <- resolveType env b
    if (conditionType == BooleanType)
        then do
            if (pathAType == pathBType)
                then return ()
                else throwError $ TypeMismatchError pathBType pathAType b
        else throwError $ TypeMismatchError conditionType BooleanType c
checkType env (LambdaExpression params body) = do
    newEnv <- newScope env params Nothing
    checkType newEnv body
checkType env (RetExpression e) = checkType env e
checkType env (TypeDeclarationExpression (TypedIdentifier i t)) = do
    setType env i t
    return ()
checkType env f@(FCallExpression i args) = do
    if i `elem` ["print", "printType"]
        then do
            mapM_ (checkType env) args
        else do
            t <- getType env i
            case t of
                (CallableType expectedParameters _) -> do
                    if (length expectedParameters /= length args)
                        then do
                            throwError $ ArgumentLengthMismatchError i
                        else do
                            ok <- foldM parameterCheck
                                        Nothing
                                        (zip expectedParameters args)
                            case ok of
                                Nothing -> return ()
                                Just e  -> throwError e
                _ -> throwError $ NotCallableError i
  where
    parameterCheck result (p, a) = do
        actual <- resolveType env a
        if (actual /= p)
            then do
                return . Just $ TypeMismatchError actual p a
            else do
                return result
checkType env ae@(AssignmentExpression i e) = do
    checkType env e
    expected <- resolveType env ae
    actual <- resolveType env e
    if (actual /= expected)
        then throwError $ TypeMismatchError actual expected ae
        else return ()
checkType env (VariableExpression i) = return ()
