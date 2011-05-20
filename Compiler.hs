module Main where

import Parser
import Lexer
import Semantic
--import Escape

import System
import IO


main = do
  args <- System.getArgs
  text <- if null args
          then hGetContents stdin
          else readFile $ head args 
  let ast = parse $ tokenize text
  print ast
  print $ semantic ast
  