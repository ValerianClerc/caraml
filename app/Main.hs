module Main where

import           Lexer
import           Lib
import           Parser
import           System.Environment
import           System.IO

main :: IO ()
main = do
  args <- getArgs
  let (cmd, filePath) = handleArgs args
  putStrLn $ "Running the following command: " ++ show cmd
    ++ ", at path: " ++ filePath
  file <- openFile filePath ReadMode
  contents <- hGetContents file
  case cmd of
    Lexer   -> print $ runLexer contents
    Parser  -> print $
      runParser . runLexer $ contents
    Invalid -> putStrLn "Invalid parameters"
