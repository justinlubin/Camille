module TypeChecker where

import Control.Concurrent.STM
import Control.Monad
import Debug.Trace

import Type
import Environment

resolveType :: Environment -> Expression -> STM (Type)
resolveType env VoidExpression = return $ CallableType [] VoidType
-- [TODO] type = last value in [expression]
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

checkType :: Environment -> Expression -> STM (Bool)
checkType env VoidExpression = return True
checkType env (BlockExpression t es) = foldM foldCheck True es
  where
    foldCheck result e = do
        retGood <- case e of
            (RetExpression _) -> do
                actual <- resolveType env e
                return $ actual == t
            _ -> return True
        check <- checkType env e
        return $ result && check && retGood
checkType env (IntegerExpression _) = return True
checkType env (StringExpression _) = return True
checkType env (BooleanExpression _) = return True
checkType env (IfExpression c a b) = do
    conditionType <- resolveType env c
    pathAType <- resolveType env a
    pathBType <- resolveType env b
    return $ conditionType == BooleanType && pathAType == pathBType
checkType env (LambdaExpression params body) = do
    newEnv <- newScope env params Nothing
    checkType newEnv body
checkType env (RetExpression e) = return True
checkType env (TypeDeclarationExpression (TypedIdentifier i t)) =
    setType env i t >> return True
checkType env (FCallExpression "print" [e]) = return True
checkType env (FCallExpression "printType" [e]) = return True
checkType env f@(FCallExpression i args) = do
    t <- getType env i
    case t of
        (CallableType expectedParameters _) -> do
            if (length expectedParameters /= length args)
                then do
                    return False
                else do
                    foldM allParametersGood
                          True
                          (zip expectedParameters args)
        _ -> return False
  where
    allParametersGood result (p, a) = do
        actual <- resolveType env a
        if (actual /= p)
            then do
                return False
            else do
                return result
checkType env ae@(AssignmentExpression i e) = do
    expected <- resolveType env ae
    actual <- resolveType env e
    return $ actual == expected
checkType env (VariableExpression i) = return True
