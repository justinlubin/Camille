module Evaluator where

import System.Environment
import Parser

eval :: Expression -> Expression
eval val@(Integer _) = val 
eval val@(String _) = val 
eval val@(Boolean _) = val 
