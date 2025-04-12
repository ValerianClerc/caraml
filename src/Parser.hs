{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Common (Type (TBool, TInt))
import Control.DeepSeq (NFData)
import Control.Exception (throw)
import Data.List.Split
import Error (ParserException (..))
import GHC.Generics (Generic)
import Lexer

data Expr
  = PInt Int
  | PChar Char
  | PBool Bool
  | PString String
  | BinOp {binOpLeft :: Expr, binOp :: Op, binOpRight :: Expr}
  | Let {letVar :: String, letEqual :: Expr}
  | Conditional {condBool :: Expr, condIf :: Expr, condElse :: Expr}
  | VarExpr {varExprName :: String}
  | FunDecl {funDeclName :: String, funDeclArgs :: [(String, Type)], funDeclExpr :: Expr}
  | FunCall {funCallName :: String, funCallArgs :: [Expr]}
  deriving (Show, Eq, NFData, Generic)

data Op = OpPlus | OpMinus | OpMult | OpDiv | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq | OpAnd | OpOr | OpNot deriving (Show, Eq, NFData, Generic)

-- program ::=
--         ((exp | decl);)+
runParser :: [Token] -> [Expr]
runParser [] = []
runParser input =
  let exprDeclList = splitOn [SC] inputToParse
   in map (checkParseResult . parseExprOrDecl) (filter (not . null) exprDeclList)
  where
    (inputToParse, _) = span (/= EOF) input

    checkParseResult :: (Expr, [Token]) -> Expr
    checkParseResult (e, []) = e
    checkParseResult (_, _) = throw $ ExtraTokens "Extra tokens found after end of expression"

parseExprOrDecl :: [Token] -> (Expr, [Token])
parseExprOrDecl [] = throw $ UnexpectedEndOfExpression "parseExprOrDecl was called with an empty token list"
-- decl ::= fun IDENT (x1: t1, x2: t2, ... xn: tn) = exp3
parseExprOrDecl (FUN : IDENT s : LPAREN : xs) =
  let (rawArgs, rest) = span (/= RPAREN) xs
      splitArgs = splitOn [COMMA] rawArgs
      getArgAndType :: [Token] -> (String, Type)
      getArgAndType [IDENT i, COLON, KBOOL] = (i, TBool)
      getArgAndType [IDENT i, COLON, KINT] = (i, TInt)
      getArgAndType t = throw $ InvalidFunctionDeclarationArgs $ "Expected identifier and type in function declaration arguments, but found: " ++ show t
      args = map getArgAndType splitArgs
      rest' = expect EQU $ expect RPAREN rest -- discard trailing RPAREN and equal sign
      (funExpr, rest'') = parseExpr rest'
   in (FunDecl {funDeclName = s, funDeclArgs = args, funDeclExpr = funExpr}, rest'')
-- else parse expression
parseExprOrDecl input =
  let (expr, rest) = parseExpr input
   in if null rest
        then (expr, [])
        else throw $ ExtraTokens "Extra tokens found after end of expression"

expect :: Token -> [Token] -> [Token]
expect t (x : xs) = if t == x then xs else throw $ ExpectedToken $ "Expected " ++ show t ++ ", found " ++ show x
expect _ [] = []

checkNext :: Token -> [Token] -> Bool
checkNext t (x : _) = t == x
checkNext _ [] = False

checkNth :: Token -> Int -> [Token] -> Bool
checkNth tokenToMatch i tokens =
  case traverse i tokens of
    Nothing -> False
    Just t -> t == tokenToMatch
  where
    traverse :: Int -> [Token] -> Maybe Token
    traverse _ [] = Nothing
    traverse 0 (t : _) = Just t
    traverse idx (t : ts) = traverse (idx - 1) ts

-- exp  ::= logical_or_exp
--         | if exp1 then exp2 else exp3
--         | let IDENT = exp1 in exp2
parseExpr :: [Token] -> (Expr, [Token])
parseExpr [] = throw $ UnexpectedEndOfExpression "parseExpr was called with an empty token list"
parseExpr (IF : xs) =
  let (ifTokens, rest) = span (/= THEN) xs
      (ifExpr, restOfIfExpr) = parseExpr ifTokens
      (thenTokens, rest') = span (/= ELSE) (expect THEN rest)
      (thenExpr, restOfThenExpr) = parseExpr thenTokens
      (elseExpr, rest'') = parseExpr (expect ELSE rest')
   in if not (null restOfIfExpr)
        then throw $ ExpectedEndOfConditional "Expected end of if-condition expression"
        else
          if not (null restOfThenExpr)
            then throw $ ExpectedEndOfConditional "Expected end of then-expression"
            else (Conditional {condBool = ifExpr, condIf = thenExpr, condElse = elseExpr}, rest'')
parseExpr (LET : IDENT s : EQU : xs) =
  (Let {letVar = s, letEqual = equalExpr}, rest)
  where
    (equalExpr, rest) = parseExpr xs
-- TODO: support let x = y in z
-- (inExpr, rest') = parseExpr (expect IN rest)
parseExpr input =
  let (expr, rest) = parseLogicalOrExpr input
   in (expr, rest)

-- logical_or_exp ::= logical_and_exp { "||" logical_and_exp }
parseLogicalOrExpr :: [Token] -> (Expr, [Token])
parseLogicalOrExpr [] = throw $ UnexpectedEndOfExpression "parseLogicalOrExpr was called with an empty token list"
parseLogicalOrExpr (x : xs) =
  let (leftExpr, rest) = parseLogicalAndExpr (x : xs)
   in case rest of
        (LOR : _) ->
          let (rightExpr, rest') = parseLogicalOrExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpOr, binOpRight = rightExpr}, rest')
        _ -> (leftExpr, rest)

-- logical_and_exp ::= equality_exp { "&&" equality_exp }
parseLogicalAndExpr :: [Token] -> (Expr, [Token])
parseLogicalAndExpr [] = throw $ UnexpectedEndOfExpression "parseLogicalAndExpr was called with an empty token list"
parseLogicalAndExpr (x : xs) =
  let (leftExpr, rest) = parseEqualityExpr (x : xs)
   in case rest of
        (LAND : _) ->
          let (rightExpr, rest') = parseLogicalAndExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpAnd, binOpRight = rightExpr}, rest')
        _ -> (leftExpr, rest)

-- equality_exp ::= relational_exp { ("!=" | "==") relational_exp }
parseEqualityExpr :: [Token] -> (Expr, [Token])
parseEqualityExpr [] = throw $ UnexpectedEndOfExpression "parseEqualityExpr was called with an empty token list"
parseEqualityExpr (x : xs) =
  let (leftExpr, rest) = parseRelationalExpr (x : xs)
   in case rest of
        (EQU : _) ->
          let (rightExpr, rest') = parseEqualityExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpEq, binOpRight = rightExpr}, rest')
        (NEQ : _) ->
          let (rightExpr, rest') = parseEqualityExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpNeq, binOpRight = rightExpr}, rest')
        _ -> (leftExpr, rest)

-- relational_exp ::= additive_exp { ("<" | ">" | "<=" | ">=") additive_exp }
parseRelationalExpr :: [Token] -> (Expr, [Token])
parseRelationalExpr [] = throw $ UnexpectedEndOfExpression "parseRelationalExpr was called with an empty token list"
parseRelationalExpr (x : xs) =
  let (leftExpr, rest) = parseAdditiveExpr (x : xs)
   in case rest of
        (LST : _) ->
          let (rightExpr, rest') = parseRelationalExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpLt, binOpRight = rightExpr}, rest')
        (GRT : _) ->
          let (rightExpr, rest') = parseRelationalExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpGt, binOpRight = rightExpr}, rest')
        (LEQ : _) ->
          let (rightExpr, rest') = parseRelationalExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpLeq, binOpRight = rightExpr}, rest')
        (GEQ : _) ->
          let (rightExpr, rest') = parseRelationalExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpGeq, binOpRight = rightExpr}, rest')
        _ -> (leftExpr, rest)

-- additive_exp ::= term { ("+" | "-") term }
parseAdditiveExpr :: [Token] -> (Expr, [Token])
parseAdditiveExpr [] = throw $ UnexpectedEndOfExpression "parseAdditiveExpr was called with an empty token list"
parseAdditiveExpr (x : xs) =
  let (leftExpr, rest) = parseTerm (x : xs)
   in case rest of
        (PLUS : _) ->
          let (rightExpr, rest') = parseAdditiveExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpPlus, binOpRight = rightExpr}, rest')
        (MINUS : _) ->
          let (rightExpr, rest') = parseAdditiveExpr (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpMinus, binOpRight = rightExpr}, rest')
        _ -> (leftExpr, rest)

-- term ::= factor { ("*" | "/") factor }
parseTerm :: [Token] -> (Expr, [Token])
parseTerm [] = throw $ UnexpectedEndOfExpression "parseTerm was called with an empty token list"
parseTerm (x : xs) =
  let (leftExpr, rest) = parseFactor (x : xs)
   in case rest of
        (ASTERISK : _) ->
          let (rightExpr, rest') = parseTerm (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpMult, binOpRight = rightExpr}, rest')
        (DIVIDE : _) ->
          let (rightExpr, rest') = parseTerm (tail rest)
           in (BinOp {binOpLeft = leftExpr, binOp = OpDiv, binOpRight = rightExpr}, rest')
        _ -> (leftExpr, rest)

-- factor ::= "(" exp ")"
--         | unary_op factor
--         | IDENT     # variable expression
--         | STRING
--         | INT
--         | CHAR
--         | BOOL
--         | x(e1, ... en)
parseFactor :: [Token] -> (Expr, [Token])
parseFactor [] = throw $ UnexpectedEndOfExpression "parseFactor was called with an empty token list"
parseFactor (LPAREN : xs) =
  let (expr, rest) = parseExpr xs
   in if checkNext RPAREN rest
        then (expr, tail rest)
        else throw $ ExpectedToken "Expected RPAREN after expression"
parseFactor (IDENT s : LPAREN : xs) =
  let (args, rest) = parseArgs xs
   in (FunCall {funCallName = s, funCallArgs = args}, rest)
  where
    parseArgs :: [Token] -> ([Expr], [Token])
    parseArgs [] = throw $ ExpectedToken "Expected RPAREN, found EOF"
    parseArgs (RPAREN : xs) = ([], xs)
    parseArgs xs =
      let (arg, rest) = parseExpr xs
       in if checkNext COMMA rest
            then
              let (args, rest') = parseArgs (tail rest)
               in (arg : args, rest')
            else ([arg], expect RPAREN rest)
parseFactor (IDENT s : xs) = (VarExpr {varExprName = s}, xs)
parseFactor (DIGIT i : xs) = (PInt i, xs)
parseFactor (CHAR c : xs) = (PChar c, xs)
parseFactor (STRING s : xs) = (PString s, xs)
parseFactor (BOOLEAN b : xs) = (PBool b, xs)
-- unary_op ::= "!" | "-" | "+"
parseFactor (NOT : xs) =
  let (expr, rest) = parseFactor xs
   in (BinOp {binOpLeft = PBool False, binOp = OpNot, binOpRight = expr}, rest)
parseFactor (MINUS : xs) =
  let (expr, rest) = parseFactor xs
   in (BinOp {binOpLeft = PInt 0, binOp = OpMinus, binOpRight = expr}, rest)
parseFactor (PLUS : xs) =
  let (expr, rest) = parseFactor xs
   in (BinOp {binOpLeft = PInt 0, binOp = OpPlus, binOpRight = expr}, rest)
parseFactor (x : _) = throw $ UnexpectedToken $ "Unexpected token: " ++ show x
