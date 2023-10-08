{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module TypeInfer where

import Control.DeepSeq (NFData)
import Data.List (find, mapAccumL)
import GHC.Generics (Generic)
import Parser (Expr (BinOp, Conditional, FunCall, FunDecl, LBool, LInt, Let, VarExpr), Op (..))

data Type
  = TInt
  | TBool
  | TVoid
  -- TODO: add char, string, list, tuple, function
  deriving (Show, Eq, NFData, Generic)

data Identifier = Variable {varType :: Type, name :: String}
  deriving (Show, Eq, NFData, Generic)

data TypedExpr
  = IntTExpr Int
  | BoolTExpr Bool
  | IdentTExpr Identifier
  | LetTExpr {letVar :: Identifier, letEqual :: TypedExpr}
  | IfTExpr {exprType :: Type, condBool :: TypedExpr, condIf :: TypedExpr, condElse :: TypedExpr}
  | BinOpTExpr {exprType :: Type, binOpLeft :: TypedExpr, binOp :: Op, binOpRight :: TypedExpr}
  deriving (Show, Eq, NFData, Generic)

-- TODO: add function declaration and function call
-- \| FunDecl {funDeclIdent :: Identifier, funDeclArgs :: [Identifier], funDeclExpr :: TypedExpr}
-- \| FunCall {funCallIdent :: Identifier, funCallArgs :: [TypedExpr]}

newtype TypeEnv = TypeEnv [Identifier]
  deriving (Show, Eq, NFData, Generic)

addToEnv :: TypeEnv -> Identifier -> TypeEnv
addToEnv (TypeEnv bindings) ident = TypeEnv $ ident : bindings

getVarType :: TypeEnv -> String -> Maybe Type
getVarType (TypeEnv bindings) ident =
  case find (\x -> name x == ident) bindings of
    Nothing -> Nothing
    Just i -> Just $ varType i

checkBinOpType :: Type -> Op -> Type -> Type
checkBinOpType t1 op t2
  | op `elem` [OpPlus, OpMinus, OpMult, OpDiv, OpLt, OpGt, OpLeq, OpGeq] =
      if t1 == TInt && t2 == TInt
        then TInt
        else error $ "Expected both operands to be of type Int, got " ++ show t1 ++ " and " ++ show t2
  | op `elem` [OpEq, OpNeq] =
      if t1 == t2
        then TBool
        else error $ "Expected both operands to be of same type, got " ++ show t1 ++ " and " ++ show t2
  | otherwise = error $ "Unsupported binary operator " ++ show op ++ " for types " ++ show t1 ++ " and " ++ show t2

runTyper :: [Expr] -> [TypedExpr]
runTyper exprs = map extractTexpr typedResult
  where
    extractTexpr :: (TypedExpr, Type, TypeEnv) -> TypedExpr
    extractTexpr result = let (texpr, _, _) = result in texpr

    -- type each expr in the list, accumulating the type environment
    (_, typedResult) =
      mapAccumL
        ( \tenv expr ->
            let (texpr, t, tenv') = typeExpr tenv expr
             in (tenv', (texpr, t, tenv'))
        )
        (TypeEnv [])
        exprs

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
typeExpr env (Parser.BinOp left op right) =
  (BinOpTExpr resultType typedLeftExpr op typedRightExpr, resultType, env)
  where
    (typedLeftExpr, tLeft, _) = typeExpr env left
    (typedRightExpr, tRight, _) = typeExpr env right
    resultType = checkBinOpType tLeft op tRight
typeExpr env (Parser.FunDecl name args expr) = undefined
typeExpr env (Parser.FunCall name args) = undefined
typeExpr env expr = error $ "Type inference not implemented for " ++ show expr