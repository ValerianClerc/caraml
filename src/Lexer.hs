{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Lexer where

import Control.DeepSeq (NFData, force)
import Data.Char
import GHC.Generics (Generic)

data Token
  = LPAREN
  | RPAREN
  | FUN
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
  | REC
  | COMMA
  | DOT
  | UNDERSCORE
  | SC
  | COLON
  | EOF
  | IDENT String
  | DIGIT Int
  | STRING String
  | CHAR Char
  | BOOLEAN Bool
  | KBOOL -- Keyword boolean
  | KINT -- Keyword int
  deriving (Show, Eq, NFData, Generic)

runLex :: String -> [Token]
runLex [] = [EOF]
runLex input@(x : xs)
  | isSpace x = runLex xs
  | isDigit x = lexDigit input
  | isAlpha x = lexAlpha input
  | otherwise = lexSymbol input

lexSymbol :: String -> [Token]
lexSymbol [] = [EOF]
lexSymbol input@(x : xs) = case x of
  '(' -> lexParens xs -- check for comments with (*
  ')' -> RPAREN : runLex xs
  '"' -> lexString input
  '\'' -> lexChar input
  '-' -> MINUS : runLex xs
  '+' -> PLUS : runLex xs
  '*' -> ASTERISK : runLex xs
  '/' -> DIVIDE : runLex xs
  '=' -> EQU : runLex xs
  '>' ->
    if null xs
      then GRT : [EOF]
      else
        let y : ys = xs
         in case y of
              '=' -> GEQ : runLex ys
              _ -> GRT : runLex xs
  '<' ->
    if null xs
      then LST : [EOF]
      else
        let y : ys = xs
         in case y of
              '=' -> LEQ : runLex ys
              '>' -> NEQ : runLex ys
              _ -> LST : runLex xs
  ',' -> COMMA : runLex xs
  '_' -> UNDERSCORE : runLex xs
  ';' -> SC : runLex xs
  ':' -> COLON : runLex xs
  _ -> error $ "Invalid character: " ++ [x]

lexParens :: String -> [Token]
lexParens [] = [LPAREN, EOF]
lexParens ('*' : xs) = runLex . goToClosingComment $ xs
  where
    goToClosingComment :: String -> String
    goToClosingComment ('*' : ')' : ys) = ys
    goToClosingComment (_ : ys) = goToClosingComment ys
    goToClosingComment [] = []
lexParens l = LPAREN : runLex l

lexString :: String -> [Token]
lexString ('"' : xs)
  | null rest = error "String was never closed"
  | otherwise = STRING strContents : runLex (tail rest)
  where
    (strContents, rest) = span (/= '"') xs
lexString _ = error "lexString called with a String not starting in \""

lexChar :: String -> [Token]
lexChar ('\'' : '\\' : x : '\'' : xs) = CHAR (matchEscapeChar x) : runLex xs -- escape characters
  where
    matchEscapeChar c = case c of
      'n' -> '\n'
      't' -> '\t'
      'r' -> '\r'
      'b' -> '\b'
      _ -> error $ "Invalid escape char: \\" ++ [x]
lexChar ('\'' : x : '\'' : xs) = CHAR x : runLex xs
lexChar ('\'' : x : _) = error $ "Invalid char: " ++ [x]
lexChar _ = error "Invalid character literal"

lexAlpha :: String -> [Token]
lexAlpha [] = [EOF]
lexAlpha s@(x : _)
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
      | str == "rec" = REC
      | str == "fun" = FUN
      | str == "bool" = KBOOL
      | str == "int" = KINT
      | str == "true" = BOOLEAN True
      | str == "false" = BOOLEAN False
      | otherwise = IDENT str

lexDigit :: String -> [Token]
lexDigit [] = [EOF]
lexDigit s = DIGIT (read digit) : runLex rest
  where
    (digit, rest) = span isDigit s
