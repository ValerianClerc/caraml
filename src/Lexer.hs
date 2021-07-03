module Lexer where

import           Data.Char

data Token =
  LParen
  | RPAREN
  | NOT
  | MINUS
  | PLUS
  | ASTERISK
  | DIVIDE
  | EQU
  | NEQ
  | GEQ
  | LEQ
  | GRT
  | LST
  | IF
  | THEN
  | ELSE
  | LET
  | IN
  | REC
  | COMMA
  | DOT
  | UNDERSCORE
  | SC
  | EOF
  | IDENT String
  | DIGIT Int
  | STRING String
  | CHAR Char
  | BOOLEAN Bool
  deriving (Show, Eq)

whitespace :: [Char]
whitespace = [' ', '\n', '\r', '\t']

runLex :: String -> [Token]
runLex [] = [EOF]
runLex input@(x:xs)
  | isSpace x = runLex xs
  | isDigit x = lexDigit input
  | isAlphaNum x = lexAlphaNum input
  | otherwise = case x of
      '(' -> parseParens input    -- check for comments with (*
      ')' -> RPAREN : runLex xs
      '-' -> MINUS : runLex xs
      '+' -> PLUS : runLex xs
      '*' -> ASTERISK : runLex xs
      '/' -> DIVIDE : runLex xs
      '=' -> EQU : runLex xs
      '>' -> GRT : runLex xs
      '<' -> LST : runLex xs
      ',' -> COMMA : runLex xs
      '_' -> UNDERSCORE : runLex xs
      ';' -> COMMA : runLex xs
      _   -> error $ "Invalid character: " ++ [x]
      -- TODO: <=, <>, >=

-- (x1s, x2s)= span isAlpha input

parseParens :: String -> [Token]
parseParens = undefined

lexAlphaNum :: String -> [Token]
lexAlphaNum = undefined

lexDigit :: String -> [Token]
lexDigit [] = [EOF]
lexDigit s = DIGIT (read digit) : runLex rest
  where
    (digit, rest) = span isDigit s
