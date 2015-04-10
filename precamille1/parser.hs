module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec

type Identifier = String
type Type = String
data Expression = Integer Integer
                | String String
                | Boolean Bool
                | TypeDeclaration Identifier Type
                | Lambda [Identifier] [Expression]
                | FCall Identifier [Expression]
                deriving (Show)

spaces1 :: Parser ()
spaces1 = skipMany1 space

quote :: Parser Char
quote = char '"'

underscore :: Parser Char
underscore = char '_'

identifier :: Parser Identifier
identifier = do first <- letter <|> underscore
                rest  <- many (alphaNum <|> underscore)
                return $ first : rest

langType :: Parser Type
langType = string "Integer" <|> string "String" <|> string "Boolean"
 
integerExpression :: Parser Expression
integerExpression = many1 digit >>= return . Integer . read

stringExpression :: Parser Expression
stringExpression = do quote
                      s <- many (noneOf ['"'])
                      quote
                      return $ String s

booleanExpression :: Parser Expression
booleanExpression = do x <- string "False" <|> string "True"
                       return $ case x of "False" -> Boolean False
                                          "True"  -> Boolean True

typeDeclarationExpression :: Parser Expression
typeDeclarationExpression = do i <- identifier
                               spaces >> string "::" >> spaces
                               t <- langType
                               return $ TypeDeclaration i t

lambdaExpression :: Parser Expression
lambdaExpression = do char '\\' >> char '(' >> spaces
                      params <- identifier
                                `sepBy` try (spaces >> char ',' >> spaces)
                      spaces >> char ')' >> spaces >> char '{' >> spaces
                      body <- expression
                              `sepBy` try (do spaces
                                              oneOf ['\n', ';']
                                              spaces)
                      spaces >> char '}'
                      return $ Lambda params body

fCallExpression :: Parser Expression
fCallExpression = do fName <- identifier
                     spaces >> char '(' >> spaces
                     args <- expression
                             `sepBy` try (spaces >> char ',' >> spaces)
                     spaces >> char ')'
                     return $ FCall fName args

expression :: Parser Expression
expression =  integerExpression
          <|> stringExpression
          <|> booleanExpression
          <|> lambdaExpression
          <|> (try typeDeclarationExpression)
          <|> (try fCallExpression)

readExpression :: String -> Either ParseError Expression
readExpression input = parse expression "" input
