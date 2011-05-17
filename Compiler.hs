module Main where

import Parser
import Lexer
import AST
import System
import IO


main = do
  args <- System.getArgs
  text <- if null args
          then hGetContents stdin
          else readFile $ head args 
  print $ parse $ tokenize text