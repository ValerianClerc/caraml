module Parser where

import           Data.List.Split
import           Lexer


data Expr =
  LInt Int | LChar Char | LBool Bool | LString String
  | BinOp Expr Op Expr
  | Let {letVar :: String, letEqual :: Expr, letIn :: Expr}
  | Conditional {condBool:: Expr, condIf:: Expr, condElse :: Expr}
  | VarExpr {varExprName :: String}
  | FunDecl {funDeclName :: String, funDeclArgs :: [String], funDeclExpr :: Expr}
  | FunCall {funCallName :: String, funCallArgs :: [Expr]}
  deriving (Show, Eq)

data Op = OpPlus | OpMinus | OpMult | OpDiv | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq deriving (Show, Eq)

runParser :: [Token] -> [Expr]
runParser [] = []
runParser (SC:xs) = runParser xs
runParser input =
  let exprList = splitOn [SC] input in
    map (\x -> let (x', _) = parseExpr x in x') exprList

-- TODO: remove left recursion from grammar

expect :: Token -> [Token] -> [Token]
expect t (x:xs) = if t == x then xs else error $ "Expected " ++ show t ++ ", found " ++ show x
expect _ [] = []

checkNext :: Token -> [Token] -> Bool
checkNext t (x:_) = t == x
checkNext _ []    = False

parseExpr :: [Token] -> (Expr, [Token])
parseExpr []    = error "parseExpr was called with an empty token list"
parseExpr ((DIGIT i):xs) = (LInt i, xs)
parseExpr ((CHAR c):xs) = (LChar c, xs)
parseExpr ((STRING s):xs) = (LString s, xs)
parseExpr ((BOOLEAN b):xs) = (LBool b, xs)
parseExpr ((IDENT s):xs) = (VarExpr {varExprName = s}, xs)
parseExpr (FUN:(IDENT s):LPAREN:xs) = (FunDecl {funDeclName = s, funDeclArgs = strArgs, funDeclExpr = funExpr}, rest'')
  where
    -- parsing args from funDecl
    (rawArgs, rest) = span (/= RPAREN) xs
    args = concat $ splitOn [COMMA] rawArgs  -- TODO: make funDeclArgs [Token] => [String] (they should all be IDENT Tokens anyways, just need to extract)
    isIdent (IDENT i) = i
    isIdent t = error $ "Expected identifier in function declaration arguments, but found: " ++ show t
    strArgs = map isIdent args

    rest' = expect RPAREN rest -- discard trailing RPAREN
    (funExpr, rest'') = parseExpr rest' -- parse function body
parseExpr (LET:(IDENT s):EQU:xs) =
  if head rest == IN then
    let
      (inExpr, rest') = parseExpr $ tail rest
      in
      (Let {letVar = s, letEqual = equalExpr, letIn = inExpr}, rest')
  else error "expected \"in\" after let expression"
  where
    (equalExpr, rest) = parseExpr xs
parseExpr input = error $ "Unimplemented parser case: " ++ show input
