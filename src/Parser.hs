{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Control.DeepSeq (NFData)
import Data.List.Split
import GHC.Generics (Generic)
import Lexer

data Expr
  = LInt Int
  | LChar Char
  | LBool Bool
  | LString String
  | BinOp {binOpLeft :: Expr, binOp :: Op, binOpRight :: Expr}
  | Let {letVar :: String, letEqual :: Expr, letIn :: Expr}
  | Conditional {condBool :: Expr, condIf :: Expr, condElse :: Expr}
  | VarExpr {varExprName :: String}
  | FunDecl {funDeclName :: String, funDeclArgs :: [String], funDeclExpr :: Expr}
  | FunCall {funCallName :: String, funCallArgs :: [Expr]}
  deriving (Show, Eq, NFData, Generic)

data Op = OpPlus | OpMinus | OpMult | OpDiv | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq deriving (Show, Eq, NFData, Generic)

runParser :: [Token] -> [Expr]
runParser [] = []
runParser (SC : xs) = runParser xs
runParser input =
  let exprList = splitOn [SC] inputToParse
   in map (checkParseResult . parseExpr) (filter (not . null) exprList)
  where
    (inputToParse, _) = span (/= EOF) input

    checkParseResult :: (Expr, [Token]) -> Expr
    checkParseResult (e, []) = e
    checkParseResult (_, _) = error "Extra tokens found after end of expression"

expect :: Token -> [Token] -> [Token]
expect t (x : xs) = if t == x then xs else error $ "Expected " ++ show t ++ ", found " ++ show x
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

parseExpr :: [Token] -> (Expr, [Token])
parseExpr [] = error "parseExpr was called with an empty token list"
-- parse literals
parseExpr ((DIGIT i) : xs) = parseExprPrime (LInt i) xs
parseExpr ((CHAR c) : xs) = parseExprPrime (LChar c) xs
parseExpr ((STRING s) : xs) = parseExprPrime (LString s) xs
parseExpr ((BOOLEAN b) : xs) = parseExprPrime (LBool b) xs
-- parsing function application
parseExpr ((IDENT s) : LPAREN : xs) =
  let (args, rest) = parseArgs xs
   in parseExprPrime (FunCall {funCallName = s, funCallArgs = args}) rest
  where
    parseArgs :: [Token] -> ([Expr], [Token])
    parseArgs [] = ([], [])
    parseArgs (RPAREN : xs) = ([], xs)
    parseArgs xs =
      let (arg, rest) = parseExpr xs
       in if checkNext COMMA rest
            then
              let (args, rest') = parseArgs (tail rest)
               in (arg : args, rest')
            else ([arg], expect RPAREN rest)
-- parsing variable expression
parseExpr ((IDENT s) : xs) = parseExprPrime (VarExpr {varExprName = s}) xs
-- parsing function declaration
parseExpr (FUN : (IDENT s) : LPAREN : xs) = parseExprPrime (FunDecl {funDeclName = s, funDeclArgs = strArgs, funDeclExpr = funExpr}) rest''
  where
    (rawArgs, rest) = span (/= RPAREN) xs
    splitArgs = splitOn [COMMA] rawArgs

    assertAndAppend :: [Token] -> [Token] -> [Token]
    assertAndAppend ts newToken =
      if length newToken == 1
        then ts ++ newToken
        else error "Expected only one expression between each comma in function declarations"

    args = foldl assertAndAppend [] splitArgs

    getIdentName :: Token -> String
    getIdentName (IDENT i) = i
    getIdentName t = error $ "Expected identifier in function declaration arguments, but found: " ++ show t
    strArgs = map getIdentName args

    rest' = expect EQU $ expect RPAREN rest -- discard trailing RPAREN and equal sign
    (funExpr, rest'') = parseExpr rest' -- parse function body

-- parsing variable definition
parseExpr (LET : (IDENT s) : EQU : xs) =
  let (inExpr, rest') = parseExpr $ expect IN rest
   in parseExprPrime (Let {letVar = s, letEqual = equalExpr, letIn = inExpr}) rest'
  where
    (equalExpr, rest) = parseExpr xs

-- parsing if-then-else
-- TODO: handle nested ifs (if (if x then true else false) then 1 else 2)
parseExpr (IF : xs) =
  if not (null restOfIfExpr) || not (null restOfThenExpr)
    then error "Expected end of expression in if-then statement"
    else parseExprPrime (Conditional {condBool = ifExpr, condIf = thenExpr, condElse = elseExpr}) rest''
  where
    (ifTokens, rest) = span (/= THEN) xs
    (ifExpr, restOfIfExpr) = parseExpr ifTokens
    (thenTokens, rest') = span (/= ELSE) (expect THEN rest)
    (thenExpr, restOfThenExpr) = parseExpr thenTokens
    (elseExpr, rest'') = parseExpr (expect ELSE rest')
parseExpr input = error $ "Unimplemented parser case: " ++ show input

isBinOp :: Token -> Maybe Op
isBinOp PLUS = Just OpPlus
isBinOp MINUS = Just OpMinus
isBinOp ASTERISK = Just OpMult
isBinOp DIVIDE = Just OpDiv
isBinOp EQU = Just OpEq
isBinOp NEQ = Just OpNeq
isBinOp GRT = Just OpGt
isBinOp GEQ = Just OpGeq
isBinOp LST = Just OpLt
isBinOp LEQ = Just OpLeq
isBinOp _ = Nothing

parseExprPrime :: Expr -> [Token] -> (Expr, [Token])
parseExprPrime e tokens@(x : xs) =
  case isBinOp x of
    Just op ->
      let (expr, rest) = parseExpr xs
       in (BinOp {binOpLeft = e, binOp = op, binOpRight = expr}, rest)
    Nothing -> (e, tokens)
parseExprPrime e [] = (e, [])