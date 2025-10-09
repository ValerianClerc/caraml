module Main where

import Compile (compileAndRun)
import qualified Data.ByteString as BS
import Lexer
import Lib
import Parser
import System.Environment
import System.FilePath
import System.Directory (copyFile)
import System.IO
import Text.Read (readMaybe)
import ToLlvm (printLlvm, toLLVM)
import TypeInfer (runTyper)
import Paths_caraml (getDataFileName)

withSource :: FilePath -> (String -> IO a) -> IO a
withSource fp k = do
  src <- readFile fp
  k src

helpMessage :: String
helpMessage = unlines
  [ "Usage: caraml <command> <file-path>"
  , "Commands:"
  , "  --emit-runtime <file-path>     Copy the runtime C file to the specified path"
  , "  --lexer <file-path>            Run the lexer on the specified file and print tokens"
  , "  --parser <file-path>           Run the parser on the specified file and print the AST"
  , "  --typecheck <file-path>        Run type checking on the specified file and print the typed AST"
  , "  --llvm <file-path>             Compile the specified file to LLVM IR and save it as a .ll file"
  , "  --compile-and-run <file-path>  Compile and run the specified file, printing the result"
  , "  --help                         Show this help message"
  ]

main :: IO ()
main = do
  args <- getArgs
  let (cmd, filePath) = handleArgs args
  putStrLn $
    "Running the following command: "
      ++ show cmd
      ++ ", at path: "
      ++ filePath
  let pipeline = runTyper . runParser . runLex
  case cmd of
    EmitRuntime -> do
      runtimePath <- getDataFileName "runtime/runtime.c"
      copyFile runtimePath filePath
      putStrLn $ "Runtime copied to " ++ filePath
    Lexer -> withSource filePath $ print . runLex
    Parser -> withSource filePath $ print . runParser . runLex
    TypeCheck -> withSource filePath $ print . pipeline
    ToLlvm -> withSource filePath $ \source -> do
      let typed = pipeline source
      llvmIrString <- printLlvm (toLLVM typed)
      let outputFilePath = replaceExtension filePath ".ll"
      BS.writeFile outputFilePath llvmIrString
      putStrLn $ "LLVM IR written to " ++ outputFilePath
    CompileAndRun -> withSource filePath $ \source -> do
      let typed = pipeline source
      result <- compileAndRun typed
      case result of
        Just val -> putStrLn $ "Result: " ++ show val
        Nothing -> putStrLn "Compilation or execution failed"
    Help -> putStrLn helpMessage
    Invalid -> putStrLn "Invalid parameters"
