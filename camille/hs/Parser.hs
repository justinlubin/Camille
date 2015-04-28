module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (space, spaces)

import Type

space :: Parser ()
space = oneOf [' ', '\t'] >> return ()

spaces :: Parser ()
spaces = skipMany space

spaces1 :: Parser ()
spaces1 = skipMany1 space

quote :: Parser Char
quote = char '"'

identifier :: Parser Identifier
identifier = do first <- letter <|> char '_'
                rest  <- many (alphaNum <|> oneOf ['_', '?'])
                return $ first : rest

langType :: Parser Type
langType = do t <-     string "Nothing"
                   <|> string "Integer"
                   <|> string "String"
                   <|> string "Boolean"
              return $ case t of "Nothing" -> NothingType
                                 "Integer" -> IntegerType
                                 "String"  -> StringType
                                 "Boolean" -> BooleanType

typedIdentifier :: Parser TypedIdentifier
typedIdentifier = do t <- langType
                     spaces1
                     i <- identifier
                     return $ TypedIdentifier i t

nothingExpression :: Parser Expression
nothingExpression = string "Nothing" >> return NothingExpression

blockExpression :: Parser Expression
blockExpression = do
    t <- langType
    spaces
    optional newline
    spaces
    char '{'
    spaces
    optional newline
    spaces
    body <- expression
            `endBy` try (do spaces
                            try (string ";\n") <|> string "\n" <|> string ";"
                            spaces)
    spaces
    optional newline
    spaces
    char '}'
    return $ BlockExpression t body

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
lambdaExpression =
    do char '\\'
       char '('
       spaces
       typedIdentifiers <- typedIdentifier
                           `sepBy` try (spaces >> char ',' >> spaces)
       spaces
       char ')'
       spaces
       string "->"
       spaces
       body <- expression
       return $ LambdaExpression typedIdentifiers body

retExpression :: Parser Expression
retExpression = do string "ret"
                   spaces1
                   val <- expression
                   return $ RetExpression val

typeDeclarationExpression :: Parser Expression
typeDeclarationExpression = do t <- langType
                               spaces1
                               i <- identifier
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
expression = do
    e <-     nothingExpression
         <|> (try blockExpression)
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
    spaces
    return e

readExpression :: String -> Either ParseError Expression
readExpression input = parse expression "" input
