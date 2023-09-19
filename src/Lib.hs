module Lib where

data Selection = Invalid | Lexer | Parser deriving (Show, Eq)

type FileName = String

handleArgs :: [String] -> (Selection, FileName)
handleArgs strs@(flag : rest)
  | length strs /= 2 = (Invalid, "")
  | flag == "--lexer" = (Lexer, head rest)
  | flag == "--parser" = (Parser, head rest)
  | otherwise = (Invalid, "")
