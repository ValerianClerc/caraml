module Main where

import Compile (compileAndRun)
import qualified Data.ByteString as BS
import Lexer
import Lib
import Parser
import System.Environment
import System.FilePath
import System.IO
import Text.Read (readMaybe)
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
        llvmIrString <- printLlvm . toLLVM . runTyper . runParser . runLex $ contents
        let outputFilePath = System.FilePath.replaceExtension filePath ".ll"
        BS.writeFile outputFilePath llvmIrString
        putStrLn $ "LLVM IR written to " ++ outputFilePath
    CompileAndRun ->
      do
        result <-
          compileAndRun
            . runTyper
            . runParser
            . runLex
            $ contents
        case result of
          Just val -> putStrLn $ "Result: " ++ show val
          Nothing -> putStrLn "Compilation or execution failed"
    Invalid -> putStrLn "Invalid parameters"
