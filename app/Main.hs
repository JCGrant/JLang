module Main where

import System.Environment
import System.IO
import Frontend.Lexer
import Frontend.Parser
import Backend.Interpreter

main :: IO ()
main = do
  args <- getArgs
  case args of
    []    -> error "Usage: jlang file"
    [arg] -> do
      handle <- openFile arg ReadMode
      contents <- hGetContents handle
      let tokens = scanTokens contents
      let ast = parse tokens
      interpret ast
    _     -> error "Too many arguments!"