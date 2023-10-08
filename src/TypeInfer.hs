module TypeInfer where

import Data.List (find)
import Parser (Expr (BinOp, Conditional, LBool, LInt, Let, VarExpr), Op)

data Type
  = TInt
  | TBool
  | TVoid
  -- TODO: add char, string, list, tuple, function
  deriving (Show, Eq)

data Identifier = Variable {varType :: Type, name :: String}
  deriving (Show, Eq)

data TypedExpr
  = IntTExpr Int
  | BoolTExpr Bool
  | IdentTExpr Identifier
  | LetTExpr {letVar :: Identifier, letEqual :: TypedExpr}
  | IfTExpr {exprType :: Type, condBool :: TypedExpr, condIf :: TypedExpr, condElse :: TypedExpr}
  | BinOpTExpr {exprType :: Type, binOpLeft :: TypedExpr, binOp :: Op, binOpRight :: TypedExpr}
  deriving (Show, Eq)

-- TODO: add function declaration and function call
-- \| FunDecl {funDeclIdent :: Identifier, funDeclArgs :: [Identifier], funDeclExpr :: TypedExpr}
-- \| FunCall {funCallIdent :: Identifier, funCallArgs :: [TypedExpr]}

data TypeBinding = Identifier
  deriving (Show, Eq)

newtype TypeEnv = TypeEnv [Identifier]
  deriving (Show, Eq)

addToEnv :: TypeEnv -> Identifier -> TypeEnv
addToEnv (TypeEnv bindings) ident = TypeEnv $ ident : bindings

getVarType :: TypeEnv -> String -> Maybe Type
getVarType (TypeEnv bindings) ident =
  case find (\x -> name x == ident) bindings of
    Nothing -> Nothing
    Just i -> Just $ varType i

runTyper :: [Expr] -> [TypedExpr]
runTyper = map (extractTexpr . typeExpr (TypeEnv []))
  where
    extractTexpr :: (TypedExpr, Type, TypeEnv) -> TypedExpr
    extractTexpr result = let (texpr, _, _) = result in texpr

typeExpr :: TypeEnv -> Parser.Expr -> (TypedExpr, Type, TypeEnv)
typeExpr env (Parser.LInt i) = (IntTExpr i, TInt, env)
typeExpr env (Parser.LBool b) = (BoolTExpr b, TBool, env)
typeExpr env (Parser.VarExpr name) =
  case getVarType env name of
    Nothing -> error $ "Variable " ++ name ++ " not found in environment"
    Just t -> (IdentTExpr $ Variable t name, t, env)
typeExpr env (Parser.Let name expr) =
  case getVarType env name of
    Nothing -> (LetTExpr var ex, TVoid, addToEnv env' var)
    Just t -> error $ "Variable " ++ name ++ " already defined in environment"
  where
    (ex, t, env') = typeExpr env expr
    var = Variable t name
typeExpr env (Parser.Conditional boolExpr ifExpr elseExpr)
  | tBool /= TBool = error $ "Expected expression in if statement to be of type Bool, got " ++ show tBool
  | tIfTExpr /= tElse = error $ "Expected return type of if/else blocks to be of same type, got " ++ show tIfTExpr ++ " and " ++ show tElse
  | otherwise = (IfTExpr tIfTExpr typedBoolExpr typedIfTExprExpr typedElseExpr, tIfTExpr, env)
  where
    (typedBoolExpr, tBool, _) = typeExpr env boolExpr
    (typedIfTExprExpr, tIfTExpr, _) = typeExpr env ifExpr
    (typedElseExpr, tElse, _) = typeExpr env elseExpr

-- typeExpr env (Parser.BinOp left op right) =
--   (BinOpTExpr tLeft typedLeftExpr op typedRightExpr, tLeft)
--   where
--     (typedLeftExpr, tLeft) = typeExpr env left
--     (typedRightExpr, tRight) = typeExpr env right