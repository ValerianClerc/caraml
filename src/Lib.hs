module Lib where

data Selection = Invalid | Lexer | Parser | TypeCheck | ToLlvm | CompileAndRun deriving (Show, Eq)

type FileName = String

handleArgs :: [String] -> (Selection, FileName)
handleArgs [] = (Invalid, "")
handleArgs strs@(flag : rest)
  | length strs /= 2 = (Invalid, "")
  | flag == "--lexer" = (Lexer, head rest)
  | flag == "--parser" = (Parser, head rest)
  | flag == "--typecheck" = (TypeCheck, head rest)
  | flag == "--llvm" = (ToLlvm, head rest)
  | flag == "--compile-and-run" = (CompileAndRun, head rest)
  | otherwise = (Invalid, "")
