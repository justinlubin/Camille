module ParseWhile where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

-- Language Grammar
-- a ::= x | n | - a | a opa a
-- b ::= true | false | not b | b opb b | a opr a
-- opa ::= + | - | * | /
-- opb ::= > | <
-- S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
           deriving (Show)

data BBinOp = And | Or deriving (Show)
data RBinOp = Greater | Less deriving (Show)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           deriving (Show)

data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            deriving (Show)

data Statement = Seq [Statement]
               | Assign String AExpr
               | If BExpr Statement Statement
               | While BExpr Statement
               | Skip
               deriving (Show)

languageDef = emptyDef { Token.commentStart = "/*"
                       , Token.commentEnd   = "*/"
                       , Token.commentLine  = "//"
                       , Token.identStart   = letter
                       , Token.identLetter  = alphaNum
                       , Token.reservedNames = [ "if"
                                               , "then"
                                               , "else"
                                               , "while"
                                               , "do"
                                               , "skip"
                                               , "true"
                                               , "false"
                                               , "not"
                                               , "and"
                                               , "or"
                                               ]
                       , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                                 , ">", "<", "and", "or", "not"
                                                 ]
                       }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Statement
whileParser = whiteSpace >> statement

statement :: Parser Statement
statement =  parens statement
         <|> sequenceOfStatements

sequenceOfStatements = do list <- (statement' `sepBy1` semi)
                          return $ if length list == 1
                                       then head list
                                       else Seq list

statement' :: Parser Statement
statement' =  ifStatement
          <|> whileStatement
          <|> skipStatement
          <|> assignStatement

ifStatement :: Parser Statement
ifStatement = do reserved "if"
                 cond <- bExpression
                 reserved "then"
                 statement1 <- statement
                 reserved "else"
                 statement2 <- statement
                 return $ If cond statement1 statement2

whileStatement :: Parser Statement
whileStatement = do reserved "while"
                    cond <- bExpression
                    reserved "do"
                    s <- statement
                    return $ While cond s

assignStatement :: Parser Statement
assignStatement = do var <- identifier
                     reservedOp ":="
                     expr <- aExpression
                     return $ Assign var expr

skipStatement :: Parser Statement
skipStatement = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-" >> return (Neg             ))          ]
             , [Infix  (reservedOp "*" >> return (ABinary Multiply)) AssocLeft]
             , [Infix  (reservedOp "/" >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+" >> return (ABinary Add     )) AssocLeft]
             , [Infix  (reservedOp "-" >> return (ABinary Subtract)) AssocLeft]
             ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not         ))          ]
             , [Infix  (reservedOp "and" >> return (BBinary And )) AssocLeft]
             , [Infix  (reservedOp "or"  >> return (BBinary Or  )) AssocLeft]
             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true" >> return (BoolConst True))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression = do a  <- aExpression
                 op <- relation
                 b  <- aExpression
                 return $ RBinary op a b

relation =  (reservedOp ">" >> return Greater)
        <|> (reservedOp "<" >> return Less)

-- REPL --

parseString :: String -> Statement
parseString s = case parse whileParser "" s of Left err  -> error $ show err
                                               Right val -> val
