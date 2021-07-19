module Parser where

import           Lexer


data Expr =
  Literal
  | BinOp Expr Op Expr
  | Let {letVar :: String, letEqual :: Expr, letIn :: Expr}
  | Conditional {condBool:: Expr, condIf:: Expr, condElse :: Expr}
  | VarExpr {varExprName :: String}
  | FunDecl {funDeclName :: String, funDeclArgs :: [String], funDeclExpr :: Expr}
  | FunCall {funCallName :: String, funCallArgs :: [Expr]}

data Literal = Int | Char | Bool
data Op = OpPlus | OpMinus | OpMult | OpDiv | OpEq | OpNeq | OpLt | OpGt | OpLeq | OpGeq deriving (Show, Eq)


runParser :: [Token] -> [Expr]
runParser = undefined
