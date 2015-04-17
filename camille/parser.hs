module Parser where

import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec

type Identifier = String
type Type = String
data Expression = NothingExpression
                | BlockExpression [Expression]
                | IntegerExpression Integer
                | StringExpression String
                | BooleanExpression Bool
                | IfExpression Expression Expression Expression
                | LambdaExpression [Identifier] Expression
                | RetExpression Expression
                | TypeDeclarationExpression Identifier Type
                | FCallExpression Identifier [Expression]
                | AssignmentExpression Identifier Expression
                | VariableExpression Identifier
                deriving (Eq, Ord)
instance Show Expression where
    show NothingExpression               = "Nothing"
    show (BlockExpression _)             = "<block>"
    show (IntegerExpression i)           = show i
    show (StringExpression s)            = show s
    show (BooleanExpression b)           = show b
    show (IfExpression _ _ _)            = "<if>"
    show (LambdaExpression _ _)          = "<lambda>"
    show (RetExpression e)               = "Ret (" ++ (show e) ++ ")"
    show (TypeDeclarationExpression _ _) = "<type-declaration>"
    show (FCallExpression _ _)           = "<fcall>"
    show (AssignmentExpression _ _)      = "<assignment>"
    show (VariableExpression i)          = "Var \"" ++ i ++ "\""

data LanguageError = TypeMismatchError
                   | NoSuchVariableError
                   | GenericError String
instance Error LanguageError where
    noMsg  = GenericError "An error has occurred."
    strMsg = GenericError

type IOThrowsError = ErrorT LanguageError IO

spaces1 :: Parser ()
spaces1 = skipMany1 space

quote :: Parser Char
quote = char '"'

identifier :: Parser Identifier
identifier = do first <- letter <|> char '_'
                rest  <- many (alphaNum <|> oneOf ['_', '?'])
                return $ first : rest

langType :: Parser Type
langType = string "Integer" <|> string "String" <|> string "Boolean"

nothingExpression :: Parser Expression
nothingExpression = string "Nothing" >> return NothingExpression

blockExpression :: Parser Expression
blockExpression = do char '{'
                     spaces
                     body <- expression `sepBy` try (do spaces
                                                        oneOf ['\n', ';']
                                                        spaces)
                     spaces
                     char '}'
                     return $ BlockExpression body

integerExpression :: Parser Expression
integerExpression = many1 digit >>= return . IntegerExpression . read

stringExpression :: Parser Expression
stringExpression = do quote
                      s <- many (noneOf ['"'])
                      quote
                      return $ StringExpression s

booleanExpression :: Parser Expression
booleanExpression = do x <- string "False" <|> string "True"
                       return $ BooleanExpression (x == "True")

ifExpression :: Parser Expression
ifExpression = do string "if"
                  spaces
                  char '('
                  condition <- expression
                  spaces
                  char ')'
                  spaces
                  truePath <- expression
                  falsePath <- option NothingExpression
                                      (do spaces
                                          string "else"
                                          spaces
                                          path <- expression
                                          spaces
                                          return path)
                  return $ IfExpression condition truePath falsePath

lambdaExpression :: Parser Expression
lambdaExpression = do char '\\'
                      char '('
                      spaces
                      params <- identifier
                                `sepBy` try (spaces >> char ',' >> spaces)
                      spaces
                      char ')'
                      spaces
                      string "->"
                      spaces
                      body <- expression
                      return $ LambdaExpression params body

retExpression :: Parser Expression
retExpression = do string "ret"
                   spaces1
                   val <- expression 
                   return $ RetExpression val

typeDeclarationExpression :: Parser Expression
typeDeclarationExpression = do i <- identifier
                               spaces
                               string "::"
                               spaces
                               t <- langType
                               return $ TypeDeclarationExpression i t

fCallExpression :: Parser Expression
fCallExpression = do fName <- identifier
                     spaces
                     char '('
                     spaces
                     args <- expression
                             `sepBy` try (spaces >> char ',' >> spaces)
                     spaces
                     char ')'
                     return $ FCallExpression fName args

assignmentExpression :: Parser Expression
assignmentExpression = do i <- identifier
                          spaces
                          char '='
                          spaces
                          e <- expression
                          return $ AssignmentExpression i e

variableExpression :: Parser Expression
variableExpression = identifier >>= return . VariableExpression

expression :: Parser Expression
expression =  nothingExpression
          <|> blockExpression
          <|> integerExpression
          <|> stringExpression
          <|> booleanExpression
          <|> ifExpression
          <|> lambdaExpression
          <|> retExpression
          <|> (try typeDeclarationExpression)
          <|> (try fCallExpression)
          <|> (try assignmentExpression)
          <|> (try variableExpression)

readExpression :: String -> Either ParseError Expression
readExpression input = parse expression "" input
