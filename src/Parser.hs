module Parser where

import           Lexer

data AST = Conditional | VarDecl deriving (Show, Eq)

runParser :: [Token] -> AST
runParser = undefined
