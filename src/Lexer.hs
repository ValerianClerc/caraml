{-# LANGUAGE ViewPatterns #-}

module Lexer where

import           Data.Char
import           Data.List

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
  | isAlpha x = lexAlpha input
  | otherwise = case x of
      '(' -> parseParens input    -- check for comments with (*
      ')' -> RPAREN : runLex xs
      '"' -> parseString input
      '\'' -> parseChar input
      '-' -> MINUS : runLex xs
      '+' -> PLUS : runLex xs
      '*' -> ASTERISK : runLex xs
      '/' -> DIVIDE : runLex xs
      '=' -> EQU : runLex xs
      '>' -> if null xs
        then  GRT : [EOF]
        else let y:ys = xs in
        case y of
          '=' -> GEQ : runLex ys
          _   -> GRT : runLex xs
      '<' -> if null xs then LST : [EOF]
        else let y:ys = xs in
        case y of
          '=' -> LEQ : runLex ys
          '>' -> NEQ : runLex ys
          _   -> LST : runLex xs
      ',' -> COMMA : runLex xs
      '_' -> UNDERSCORE : runLex xs
      ';' -> COMMA : runLex xs
      _   -> error $ "Invalid character: " ++ [x]

-- (x1s, x2s)= span isAlpha input

parseParens :: String -> [Token]
parseParens = undefined

parseString :: String -> [Token]
parseString = undefined

parseChar :: String -> [Token]
parseChar = undefined

lexAlpha :: String -> [Token]
lexAlpha [] = [EOF]
lexAlpha s@(x:_)
  | isAlpha x = matchPrefix ident : runLex rest
  | otherwise = error "LexAlpha: Input is not alphabetical, this should never be reached"
  where
    (ident, rest) = span isAlphaNum s

    matchPrefix :: String -> Token
    matchPrefix str
      | str == "not" = NOT
      | str == "if" = IF
      | str == "then" = THEN
      | str == "else" = ELSE
      | str == "let" = LET
      | str == "in" = IN
      | str == "rec" = REC
      | str == "let" = LET
      | str == "true" = BOOLEAN True
      | str == "false" = BOOLEAN False
      | otherwise = IDENT str

lexDigit :: String -> [Token]
lexDigit [] = [EOF]
lexDigit s = DIGIT (read digit) : runLex rest
  where
    (digit, rest) = span isDigit s
