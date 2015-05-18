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

whitespace :: Parser ()
whitespace = oneOf [' ', '\t', '\n'] >> return ()

whitespaces :: Parser ()
whitespaces = skipMany whitespace

whitespaces1 :: Parser ()
whitespaces1 = skipMany1 whitespace

quote :: Parser Char
quote = char '"'

identifier :: Parser Identifier
identifier = do first <- letter <|> char '_'
                rest  <- many (alphaNum <|> oneOf ['_', '?'])
                return $ first : rest

baseType :: Parser Type
baseType = do t <-     string "Void"
                   <|> string "Integer"
                   <|> string "String"
                   <|> string "Boolean"
              return $ case t of "Void"    -> VoidType
                                 "Integer" -> IntegerType
                                 "String"  -> StringType
                                 "Boolean" -> BooleanType

callableType :: Parser Type
callableType =
        (do char '('
            spaces
            parameterTypes <- (callableType <|> baseType)
                              `sepBy` try (spaces >> char ',' >> spaces)
            spaces
            string "->"
            spaces
            returnType <- callableType <|> baseType
            char ')'
            return $ CallableType parameterTypes returnType)
    <|> (do bt <- baseType
            return $ CallableType [] bt)

typedIdentifier :: Parser TypedIdentifier
typedIdentifier = do
    i <- identifier
    spaces
    string "::"
    spaces
    t <- callableType
    return $ TypedIdentifier i t

voidExpression :: Parser Expression
voidExpression = string "Nothing" >> return VoidExpression

blockExpression :: Parser Expression
blockExpression = do
    t <- try $ do t <- callableType
                  spaces
                  optional newline
                  spaces
                  char '{'
                  return t
    spaces
    optional newline
    spaces
    body <- expression
            `endBy` (do spaces
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
ifExpression = do try $ string "if"
                  spaces1
                  condition <- expression
                  spaces1
                  truePath <- expression
                  falsePath <- option VoidExpression
                                      (do try $ do whitespaces1
                                                   string "else"
                                          spaces1
                                          path <- expression
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
retExpression = do try $ do string "ret"
                            spaces1
                   val <- expression
                   return $ RetExpression val

typeDeclarationExpression :: Parser Expression
typeDeclarationExpression = try $ do ti <- typedIdentifier
                                     return $ TypeDeclarationExpression ti

fCallExpression :: Parser Expression
fCallExpression = do fName <- try $ do
                                  fName <- identifier
                                  spaces
                                  char '('
                                  return fName
                     spaces
                     args <- expression
                             `sepBy` try (spaces >> char ',' >> spaces)
                     spaces
                     char ')'
                     return $ FCallExpression fName args

assignmentExpression :: Parser Expression
assignmentExpression = do i <- try $ do
                                   i <- try identifier
                                   spaces
                                   char '='
                                   return i
                          spaces
                          e <- expression
                          return $ AssignmentExpression i e

variableExpression :: Parser Expression
variableExpression = identifier >>= return . VariableExpression

expression :: Parser Expression
expression = do
    e <-     voidExpression
         <|> blockExpression
         <|> integerExpression
         <|> stringExpression
         <|> booleanExpression
         <|> ifExpression
         <|> lambdaExpression
         <|> retExpression
         <|> typeDeclarationExpression
         <|> fCallExpression
         <|> assignmentExpression
         <|> variableExpression
    return e

readExpression :: String -> Either ParseError Expression
readExpression input = parse expression "" input
