module TypeChecker where

import Type
import Environment
import Evaluator

resolveType :: Environment -> Expression -> STM (Type)
resolveType env VoidExpression = return VoidType
resolveType env (IntegerExpression _) = return IntegerType
resolveType env (StringExpression _) = return StringType
resolveType env (BooleanExpression _) = return BooleanType
resolveType env (IfExpression _ a b) = resolveType env a
resolveType env (LambdaExpression _ e) = resolveType env e
resolveType env (RetExpression e) = resolveType env e
resolveType env (TypeDeclarationExpression _ _) = return VoidType
resolveType env (FCallExpression i _) = getType env i
resolveType env (AssignmentExpression i e) = getType env i
resolveType env (VariableExpression i) = getType env i

checkType :: Environment -> Expression -> Expression -> Boolean
checkType env a b = getType a == getType b
