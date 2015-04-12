module Parser where

import Control.Monad
import Text.ParserCombinators.Parsec

type Identifier = String
type Type = String
data Expression = ENothing
                | EBlock [Expression]
                | EInteger Integer
                | EString String
                | EBoolean Bool
                | EIf Expression Expression Expression
                | ELambda [Identifier] Expression
                | ERet Expression
                | ETypeDeclaration Identifier Type
                | EFCall Identifier [Expression]
                | EAssignment Identifier Expression
                | EVariable Identifier
                deriving (Eq, Show)
--instance Eq Expression where
--    ENothing     == ENothing     = True
--    (EInteger a) == (EInteger b) = a == b
--    (EString a)  == (EString b) = a == b
--    (EBoolean a) == (EBoolean b) = a == b
--    _            == _            = False

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

nothingExpression :: Parser Expression
nothingExpression = string "Nothing" >> return ENothing

blockExpression :: Parser Expression
blockExpression = do char '{'
                     spaces
                     body <- expression `sepBy` try (do spaces
                                                        oneOf ['\n', ';']
                                                        spaces)
                     spaces
                     char '}'
                     return $ EBlock body

integerExpression :: Parser Expression
integerExpression = many1 digit >>= return . EInteger . read

stringExpression :: Parser Expression
stringExpression = do quote
                      s <- many (noneOf ['"'])
                      quote
                      return $ EString s

booleanExpression :: Parser Expression
booleanExpression = do x <- string "False" <|> string "True"
                       return $ EBoolean (x == "True")

ifExpression :: Parser Expression
ifExpression = do string "if"
                  spaces
                  char '('
                  condition <- expression
                  spaces
                  char ')'
                  spaces
                  truePath <- expression
                  falsePath <- option ENothing (do spaces
                                                   string "else"
                                                   spaces
                                                   path <- expression
                                                   spaces
                                                   return path)
                  return $ EIf condition truePath falsePath

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
                      return $ ELambda params body

retExpression :: Parser Expression
retExpression = do string "ret"
                   spaces
                   val <- expression 
                   return $ ERet val

typeDeclarationExpression :: Parser Expression
typeDeclarationExpression = do i <- identifier
                               spaces
                               string "::"
                               spaces
                               t <- langType
                               return $ ETypeDeclaration i t

fCallExpression :: Parser Expression
fCallExpression = do fName <- identifier
                     spaces
                     char '('
                     spaces
                     args <- expression
                             `sepBy` try (spaces >> char ',' >> spaces)
                     spaces
                     char ')'
                     return $ EFCall fName args

assignmentExpression :: Parser Expression
assignmentExpression = do i <- identifier
                          spaces
                          char '='
                          spaces
                          e <- expression
                          return $ EAssignment i e

variableExpression :: Parser Expression
variableExpression = identifier >>= return . EVariable

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
