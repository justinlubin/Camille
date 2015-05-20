module Type where

import Control.Concurrent.STM
import Control.Monad.Error
import Data.List (intercalate)

type Identifier = String

data Type = VoidType
          | IntegerType
          | StringType
          | BooleanType
          | CallableType [Type] Type

returnType :: Type -> Type
returnType (CallableType _ rt) = rt
returnType t = t

instance Eq Type where
    VoidType               == VoidType               = True
    IntegerType            == IntegerType            = True
    StringType             == StringType             = True
    BooleanType            == BooleanType            = True
    (CallableType [] rt)   == t                      = rt == returnType t
    (CallableType ts1 rt1) == (CallableType ts2 rt2) = ts1 == ts2 && rt1 == rt2
    _                      == _                      = False
instance Show Type where
    show (VoidType) = "Void"
    show (IntegerType) = "Integer"
    show (StringType) = "String"
    show (BooleanType) = "Boolean"
    show (CallableType [] rt) = show rt
    show (CallableType ts rt) = "(" ++
                                (intercalate ", " (map show ts)) ++
                                " -> " ++
                                (show rt) ++
                                ")"

data TypedIdentifier = TypedIdentifier Identifier Type
                     deriving (Eq, Show)

data Expression = VoidExpression
                | BlockExpression Type [Expression]
                | IntegerExpression Integer
                | StringExpression String
                | BooleanExpression Bool
                | IfExpression Expression Expression Expression
                | LambdaExpression [TypedIdentifier] Expression
                | RetExpression Expression
                | TypeDeclarationExpression TypedIdentifier
                | FCallExpression Identifier [Expression]
                | AssignmentExpression Identifier Expression
                | VariableExpression Identifier
                deriving (Eq)
instance Ord Expression where
    compare VoidExpression _                         = LT
    compare (IntegerExpression a) (IntegerExpression b) = compare a b
    compare (StringExpression a) (StringExpression b)   = compare a b
    compare (BooleanExpression a) (BooleanExpression b) = compare a b
    compare _ _                                         = EQ
instance Show Expression where
    show VoidExpression                  = "Nothing"
    show (BlockExpression t _)           = "<block>(" ++ (show t) ++ ")"
    show (IntegerExpression i)           = show i
    show (StringExpression s)            = s
    show (BooleanExpression b)           = show b
    show (IfExpression _ _ _)            = "<if>"
    show (LambdaExpression _ _)          = "<lambda>"
    show (RetExpression e)               = "<ret>(" ++ (show e) ++ ")"
    show (TypeDeclarationExpression (TypedIdentifier i t))  = "<type-declaration>(" ++ i ++ " :: " ++ (show t) ++ ")"
    show (FCallExpression i es)           = "<fcall>(" ++ i ++ "(" ++ (intercalate ", " (map show es)) ++ "))"
    show (AssignmentExpression i e)      = "<assignment>(" ++ i ++ " = " ++ (show e) ++ ")"
    show (VariableExpression i)          = "<var>(" ++ i ++ ")"

data LanguageError = TypeMismatchError Type Type Expression
                   | TypeDeclarationNotFoundError Identifier
                   | TypeDeclarationAlreadyExistsError Identifier
                   | NoSuchVariableError Identifier
                   | ArgumentLengthMismatchError Identifier
                   | NotCallableError Identifier
                   | GenericError String
instance Error LanguageError where
    noMsg  = GenericError "An error has occurred."
    strMsg = GenericError
instance Show LanguageError where
    show (TypeMismatchError a e ex) = "TypeMismatchError: expected type '" ++ (show e) ++ "', got type '" ++ (show a) ++ "' in expression '" ++ (show ex) ++ "'"
    show (TypeDeclarationNotFoundError i) = "TypeDeclarationNotFoundError: " ++ i
    show (TypeDeclarationAlreadyExistsError i) = "TypeDeclarationAlreadyExistsError: " ++ i
    show (NoSuchVariableError i) = "NoSuchVariableError: " ++ i
    show (ArgumentLengthMismatchError i) = "ArgumentLengthMismatchError: " ++ i
    show (NotCallableError i) = "NotCallableError: " ++ i
    show (GenericError s) = s

type IOThrowsError = ErrorT LanguageError IO
type STMThrowsError = ErrorT LanguageError STM

stmToIO :: STMThrowsError a -> IOThrowsError a
stmToIO s = do
    r <- lift . atomically . runErrorT $ s
    case r of
        Left e -> throwError e
        Right a -> return a

runThrowsIO :: a -> IOThrowsError a -> IO a
runThrowsIO d action = do
    te <- runErrorT action
    case te of
        Left e    -> print e >> return d
        Right val -> return val

checkThrowsIO :: IOThrowsError a -> IO (Bool)
checkThrowsIO action = do
    te <- runErrorT action
    case te of
        Left e  -> print e >> return False
        Right v -> return True
