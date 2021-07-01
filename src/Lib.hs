module Lib where

data Selection = Invalid | Lexer | Parser deriving (Show, Eq)
type FileName = String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

handleArgs :: [String] -> (Selection, FileName)
handleArgs strs
  | length strs /= 2 = (Invalid, "")
  | head strs == "--lexer" = (Lexer, strs !! 1)
  | head strs == "--parser" = (Parser, strs !! 1)
  | otherwise = (Invalid, "")
