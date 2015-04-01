module Main where

import Control.Monad (liftM)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
               deriving (Show)

main :: IO ()
main = do
     args <- getArgs
     putStrLn (readExpr (args !! 0))

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
         Left err -> "No match: " ++ show err
         Right val -> "Found value: " ++ show val

--
-- LispVal Parsers
--

parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> parseNumber
          <|> parseQuoted
          <|> do char '('
                 x <- try parseList <|> parseDottedList
                 char ')'
                 return x

parseAtom :: Parser LispVal                                  
parseAtom = do first <- letter <|> symbol                    
               rest  <- many (letter <|> digit <|> symbol)   
               let atom = [first] ++ rest                    
               return $ case atom of "#t"      -> Bool True  
                                     "#f"      -> Bool False 
                                     otherwise -> Atom atom  

parseList :: Parser LispVal
parseList = liftM List $ parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many (escapedChars <|> (noneOf ['\\', '"']))
                 char '"'
                 return $ String s

--
-- Helpers
--

escapedChars :: Parser Char
escapedChars = do
             char '\\'
             c <- oneOf ['\\','"', 'n', 'r', 't']
             return $ case c of
                    '\\' -> c
                    '"'  -> c
                    'n'  -> '\n'
                    'r'  -> '\r'
                    't'  -> '\t'

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"
symbol = oneOf "!$%&|*+ -/: <=? >@^_~#"

spaces :: Parser ()
spaces = skipMany1 space
