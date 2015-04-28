module TypeChecker where

import Type
import Environment
import Evaluator

resolveType :: Environment -> Expression -> STM (Type)
resolveType env NothingExpression = return NothingType
resolveType env (IntegerExpression _) = return IntegerType
resolveType env (StringExpression _) = return StringType
resolveType env (BooleanExpression _) = return BooleanType
resolveType env (IfExpression _ a b) = resolveType env a
resolveType env (LambdaExpression _ e) = resolveType env e
resolveType env (RetExpression e) = resolveType env e
resolveType env (TypeDeclarationExpression _ _) = return NothingType
resolveType env (FCallExpression i _) = getType env i
resolveType env (AssignmentExpression i e) = getType env i
resolveType env (VariableExpression i) = getType env i
