module Type where

import Control.Concurrent.STM
import Control.Monad.Error

type Identifier = String

data Type = VoidType
          | IntegerType
          | StringType
          | BooleanType
          | CallableType [Type] Type
          deriving (Eq, Show)
returnType :: Type -> Type
returnType (CallableType _ rt) = rt
returnType t = t

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
    show (BlockExpression _ _)           = "<block>"
    show (IntegerExpression i)           = show i
    show (StringExpression s)            = s
    show (BooleanExpression b)           = show b
    show (IfExpression _ _ _)            = "<if>"
    show (LambdaExpression _ _)          = "<lambda>"
    show (RetExpression e)               = "Ret (" ++ (show e) ++ ")"
    show (TypeDeclarationExpression ti)  = "<type-declaration>"
    show (FCallExpression _ _)           = "<fcall>"
    show (AssignmentExpression _ _)      = "<assignment>"
    show (VariableExpression i)          = "Var \"" ++ i ++ "\""

data LanguageError = TypeMismatchError Type Type
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
    show (TypeMismatchError a e) = "TypeMismatchError: expected " ++ (show e) ++ ", got " ++ (show a)
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
