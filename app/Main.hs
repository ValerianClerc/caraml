module Main where

import Lexer
import Lib
import Parser
import System.Environment
import System.FilePath
import System.IO
import ToLlvm (printLlvm, toLLVM)
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
      do
        let llvmIrString = printLlvm . toLLVM . runTyper . runParser . runLex $ contents
        let outputFilePath = System.FilePath.replaceExtension filePath ".ll"
        writeFile outputFilePath llvmIrString
        putStrLn $ "LLVM IR written to " ++ outputFilePath
    Invalid -> putStrLn "Invalid parameters"
