module Main where

import Parser
import Lexer
import Semantic
import Escape
import IR
import Codegen
import AST
import qualified Unify 

import System
import IO
import Control.Monad

main = do
  args <- System.getArgs
  text <- if null args
          then hGetContents stdin
          else readFile $ head args 
  let (_, ast) = semantic $ parse $ tokenize text
      canonical = escape $ unique $ ast
      (fundefs, instrs) = translate canonical
  ast `seq` codegen instrs fundefs  -- force type errors before codegen
  
debug f = do
  args <- System.getArgs
  text <- readFile f
  let (t, ast) = semantic $ parse $ tokenize text
      canonical = escape $ unique $ ast
      (fundefs, instrs) = translate canonical
  forM_ fundefs print
  putStrLn ""
  forM_ instrs print