module Lexer where

data Token = Eq | Let deriving (Show, Eq)

runLexer :: String -> [Token]
runLexer = undefined
