module Main where

import Lexer
import Lib
import Parser
import System.Environment
import System.IO
import ToLlvm (toLLVM, printLlvm)
import TypeInfer (runTyper)

main :: IO ()
main = do
  args <- getArgs
  let (cmd, filePath) = handleArgs args
  putStrLn $
    "Running the following command: "
      ++ show cmd
      ++ ", at path: "
      ++ filePath
  file <- openFile filePath ReadMode
  contents <- hGetContents file
  case cmd of
    Lexer -> print $ runLex contents
    Parser ->
      print $
        runParser . runLex $
          contents
    TypeCheck ->
      print $
        runTyper . runParser . runLex $
          contents
    ToLlvm ->
      print $
        printLlvm . toLLVM . runTyper . runParser . runLex $
          contents
    Invalid -> putStrLn "Invalid parameters"
