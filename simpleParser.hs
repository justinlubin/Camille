module SimpleParser where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
instance Show LispVal where show = showVal

showVal :: LispVal -> String
showVal (Atom name) = name
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++
                                 " . " ++ showVal tail ++ ")"
showVal (Number n) = show n
showVal (String s) = "\"" ++ s ++ "\""
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do char '\\'
                  c <- oneOf ['\\', '"', 'n', 't']
                  return $ case c of '\\' -> '\\'
                                     '"'  -> '"'
                                     'n'  -> '\n'
                                     't'  -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 s <- many (escapedChars <|> (noneOf ['\\', '"']))
                 char '"'
                 return $ String s

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of "#t"      -> Bool True
                                     "#f"      -> Bool False
                                     otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseList :: Parser LispVal
parseList = liftM List $ parseExpression `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do head <- parseExpression `endBy` spaces
                     tail <- char '.' >> spaces >> parseExpression
                     return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpression
                 return $ List [Atom "quote", x]
                     
parseExpression :: Parser LispVal
parseExpression = parseAtom
              <|> parseString
              <|> parseNumber
              <|> parseQuoted
              <|> do char '('
                     x <- (try parseList) <|> parseDottedList
                     char ')'
                     return x

readExpression :: String -> String
readExpression input = case parse parseExpression "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpression (args !! 0))
