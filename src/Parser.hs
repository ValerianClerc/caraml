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
   in parseLogicalOrExprRest leftExpr rest
  where
    parseLogicalOrExprRest :: Expr -> [Token] -> (Expr, [Token])
    parseLogicalOrExprRest expr [] = (expr, [])
    parseLogicalOrExprRest expr (LOR : rest) =
      let (rightExpr, rest') = parseLogicalAndExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpOr, binOpRight = rightExpr}
       in parseLogicalOrExprRest newExpr rest'
    parseLogicalOrExprRest expr rest = (expr, rest)

-- logical_and_exp ::= equality_exp { "&&" equality_exp }
parseLogicalAndExpr :: [Token] -> (Expr, [Token])
parseLogicalAndExpr [] = throw $ UnexpectedEndOfExpression "parseLogicalAndExpr was called with an empty token list"
parseLogicalAndExpr (x : xs) =
  let (leftExpr, rest) = parseEqualityExpr (x : xs)
   in parseLogicalAndExprRest leftExpr rest
  where
    parseLogicalAndExprRest :: Expr -> [Token] -> (Expr, [Token])
    parseLogicalAndExprRest expr [] = (expr, [])
    parseLogicalAndExprRest expr (LAND : rest) =
      let (rightExpr, rest') = parseEqualityExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpAnd, binOpRight = rightExpr}
       in parseLogicalAndExprRest newExpr rest'
    parseLogicalAndExprRest expr rest = (expr, rest)

-- equality_exp ::= relational_exp { ("!=" | "==") relational_exp }
parseEqualityExpr :: [Token] -> (Expr, [Token])
parseEqualityExpr [] = throw $ UnexpectedEndOfExpression "parseEqualityExpr was called with an empty token list"
parseEqualityExpr (x : xs) =
  let (leftExpr, rest) = parseRelationalExpr (x : xs)
   in parseEqualityExprRest leftExpr rest
  where
    parseEqualityExprRest :: Expr -> [Token] -> (Expr, [Token])
    parseEqualityExprRest expr [] = (expr, [])
    parseEqualityExprRest expr (EQU : rest) =
      let (rightExpr, rest') = parseRelationalExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpEq, binOpRight = rightExpr}
       in parseEqualityExprRest newExpr rest'
    parseEqualityExprRest expr (NEQ : rest) =
      let (rightExpr, rest') = parseRelationalExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpNeq, binOpRight = rightExpr}
       in parseEqualityExprRest newExpr rest'
    parseEqualityExprRest expr rest = (expr, rest)

-- relational_exp ::= additive_exp { ("<" | ">" | "<=" | ">=") additive_exp }
parseRelationalExpr :: [Token] -> (Expr, [Token])
parseRelationalExpr [] = throw $ UnexpectedEndOfExpression "parseRelationalExpr was called with an empty token list"
parseRelationalExpr (x : xs) =
  let (leftExpr, rest) = parseAdditiveExpr (x : xs)
   in parseRelationalExprRest leftExpr rest
  where
    parseRelationalExprRest :: Expr -> [Token] -> (Expr, [Token])
    parseRelationalExprRest expr [] = (expr, [])
    parseRelationalExprRest expr (LST : rest) =
      let (rightExpr, rest') = parseAdditiveExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpLt, binOpRight = rightExpr}
       in parseRelationalExprRest newExpr rest'
    parseRelationalExprRest expr (GRT : rest) =
      let (rightExpr, rest') = parseAdditiveExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpGt, binOpRight = rightExpr}
       in parseRelationalExprRest newExpr rest'
    parseRelationalExprRest expr (LEQ : rest) =
      let (rightExpr, rest') = parseAdditiveExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpLeq, binOpRight = rightExpr}
       in parseRelationalExprRest newExpr rest'
    parseRelationalExprRest expr (GEQ : rest) =
      let (rightExpr, rest') = parseAdditiveExpr rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpGeq, binOpRight = rightExpr}
       in parseRelationalExprRest newExpr rest'
    parseRelationalExprRest expr rest = (expr, rest)

-- additive_exp ::= term { ("+" | "-") term }
parseAdditiveExpr :: [Token] -> (Expr, [Token])
parseAdditiveExpr [] = throw $ UnexpectedEndOfExpression "parseAdditiveExpr was called with an empty token list"
parseAdditiveExpr (x : xs) =
  let (leftExpr, rest) = parseTerm (x : xs)
   in parseAdditiveExprRest leftExpr rest
  where
    parseAdditiveExprRest :: Expr -> [Token] -> (Expr, [Token])
    parseAdditiveExprRest expr [] = (expr, [])
    parseAdditiveExprRest expr (PLUS : rest) =
      let (rightExpr, rest') = parseTerm rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpPlus, binOpRight = rightExpr}
       in parseAdditiveExprRest newExpr rest'
    parseAdditiveExprRest expr (MINUS : rest) =
      let (rightExpr, rest') = parseTerm rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpMinus, binOpRight = rightExpr}
       in parseAdditiveExprRest newExpr rest'
    parseAdditiveExprRest expr rest = (expr, rest)

-- term ::= factor { ("*" | "/") factor }
parseTerm :: [Token] -> (Expr, [Token])
parseTerm [] = throw $ UnexpectedEndOfExpression "parseTerm was called with an empty token list"
parseTerm (x : xs) =
  let (leftExpr, rest) = parseFactor (x : xs)
   in parseTermRest leftExpr rest
  where
    parseTermRest :: Expr -> [Token] -> (Expr, [Token])
    parseTermRest expr [] = (expr, [])
    parseTermRest expr (ASTERISK : rest) =
      let (rightExpr, rest') = parseFactor rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpMult, binOpRight = rightExpr}
       in parseTermRest newExpr rest'
    parseTermRest expr (DIVIDE : rest) =
      let (rightExpr, rest') = parseFactor rest
          newExpr = BinOp {binOpLeft = expr, binOp = OpDiv, binOpRight = rightExpr}
       in parseTermRest newExpr rest'
    parseTermRest expr rest = (expr, rest)

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
