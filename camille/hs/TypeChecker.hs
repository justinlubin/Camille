module TypeChecker where

import Control.Concurrent.STM
import Control.Monad

import Type
import Environment
import Evaluator

resolveType :: Environment -> Expression -> STM (Type)
resolveType env VoidExpression = return VoidType
-- [TODO] type = last value in [expression]
resolveType env (BlockExpression t _) = return t
resolveType env (IntegerExpression _) = return IntegerType
resolveType env (StringExpression _) = return StringType
resolveType env (BooleanExpression _) = return BooleanType
resolveType env (IfExpression _ a _) = resolveType env a
resolveType env (LambdaExpression _ e) = resolveType env e
resolveType env (RetExpression e) = resolveType env e
resolveType env (TypeDeclarationExpression _) = return VoidType
resolveType env (FCallExpression i _) = getType env i
resolveType env (AssignmentExpression i e) = resolveType e
resolveType env (VariableExpression i) = getType env i

checkType :: Environment -> Expression -> STM (Bool)
checkType env VoidExpression = return True
checkType env (BlockExpression t es) = fold allRetsGood (return True) es
    where allRetsGood result e = do
        if (e == RetExpression)
            then do
                actual <- resolveType e
                return $ actual == t
            else do
                result
checkType env (IntegerExpression _) = return True
checkType env (StringExpression _) = return True
checkType env (BooleanExpression _) = return True
checkType env (IfExpression c a b) = do
    conditionType <- resolveType c
    pathAType <- resolveType a
    pathBType <- resolveType b
    return $ conditionType == BooleanType && pathAType == pathBType
checkType env (LambdaExpression _ _) = return True
checkType env (RetExpression e) = return True
resolveType env (TypeDeclarationExpression _) = return True
resolveType env (FCallExpression i es) = undefined -- [TODO]
