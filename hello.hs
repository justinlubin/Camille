module Main where
import System.Environment

main :: IO ()
main = do name <- getLine
          putStrLn ("Hi, " ++ name ++ "!")
