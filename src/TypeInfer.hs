{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module TypeInfer where

import Common (Type (..))
import Control.DeepSeq (NFData)
import Data.List (find, mapAccumL)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Parser (Expr (BinOp, Conditional, FunCall, FunDecl, Let, PBool, PInt, VarExpr), Op (..))

data Identifier = Variable {varType :: Type, name :: String}
  deriving (Show, Eq, NFData, Generic)

data TypedExpr
  = IntTExpr Int
  | BoolTExpr Bool
  | IdentTExpr Identifier
  | LetTExpr {letVar :: Identifier, letEqual :: TypedExpr}
  | IfTExpr {exprType :: Type, condBool :: TypedExpr, condIf :: TypedExpr, condElse :: TypedExpr}
  | BinOpTExpr {exprType :: Type, binOpLeft :: TypedExpr, binOp :: Op, binOpRight :: TypedExpr}
  | FunDeclTExpr {funDeclIdent :: Identifier, funDeclTArgs :: [Identifier], funDeclTExpr :: TypedExpr}
  | FunCallTExpr {funCallIdent :: Identifier, funCallTArgs :: [TypedExpr]}
  deriving (Show, Eq, NFData, Generic)

newtype TypeEnv = TypeEnv [Identifier]
  deriving (Show, Eq, NFData, Generic)

addToEnv :: TypeEnv -> Identifier -> TypeEnv
addToEnv env@(TypeEnv bindings) ident@(Variable itype iname) =
  case getVarType env iname of
    Nothing -> TypeEnv (ident : bindings)
    Just t -> error $ "Variable " ++ iname ++ " already defined in environment"

getVarType :: TypeEnv -> String -> Maybe Type
getVarType (TypeEnv bindings) ident =
  case find (\x -> name x == ident) bindings of
    Nothing -> Nothing
    Just i -> Just $ varType i

checkBinOpType :: Type -> Op -> Type -> Type
checkBinOpType t1 op t2
  | t1 == TUnknown && t2 == TUnknown = TUnknown
  | op `elem` [OpPlus, OpMinus, OpMult, OpDiv] =
      if t1 == TInt && t2 == TInt || t1 == TUnknown && t2 == TInt || t1 == TInt && t2 == TUnknown
        then TInt
        else error $ "Expected both operands to be of type Int, got " ++ show t1 ++ " and " ++ show t2
  | op `elem` [OpLt, OpGt, OpLeq, OpGeq] =
      if t1 == TInt && t2 == TInt || t1 == TUnknown && t2 == TInt || t1 == TInt && t2 == TUnknown
        then TBool
        else error $ "Expected both operands to be of type Int, got " ++ show t1 ++ " and " ++ show t2
  | op `elem` [OpEq, OpNeq] =
      if t1 == t2 || t1 == TUnknown || t2 == TUnknown
        then TBool
        else error $ "Expected both operands to be of same type, got " ++ show t1 ++ " and " ++ show t2
  | op `elem` [OpAnd, OpOr] =
      if t1 == TBool && t2 == TBool || t1 == TUnknown && t2 == TBool || t1 == TBool && t2 == TUnknown
        then TBool
        else error $ "Expected both operands to be of type Bool, got " ++ show t1 ++ " and " ++ show t2
  | otherwise = error $ "Unsupported binary operator " ++ show op ++ " for types " ++ show t1 ++ " and " ++ show t2

runTyper :: [Expr] -> [TypedExpr]
runTyper exprs = map checkUnknowns typedResult
  where
    -- type each expr in the list, accumulating the type environment
    (_, typedResult) =
      mapAccumL
        ( \tenv expr ->
            let (texpr, t, tenv') = typeExpr tenv expr
             in (tenv', (texpr, t, tenv'))
        )
        (TypeEnv [])
        exprs

    checkUnknowns res = let (texpr, _, env) = res in checkForUnknowns env texpr

typeExpr :: TypeEnv -> Parser.Expr -> (TypedExpr, Type, TypeEnv)
typeExpr env (Parser.PInt i) = (IntTExpr i, TInt, env)
typeExpr env (Parser.PBool b) = (BoolTExpr b, TBool, env)
typeExpr env (Parser.VarExpr name) =
  case getVarType env name of
    Nothing -> error $ "Variable " ++ name ++ " not found in environment"
    Just t -> (IdentTExpr $ Variable t name, t, env)
typeExpr env (Parser.Let name expr) =
  (LetTExpr var ex, TVoid, addToEnv env var)
  where
    (ex, t, _) = typeExpr env expr
    var = Variable t name
typeExpr env (Parser.Conditional boolExpr ifExpr elseExpr)
  | tBool /= TBool = error $ "Expected expression in if statement to be of type Bool, got " ++ show tBool
  | tIfTExpr == TUnknown = (IfTExpr tElse typedBoolExpr typedIfTExprExpr typedElseExpr, tElse, env)
  | tElse == TUnknown = (IfTExpr tIfTExpr typedBoolExpr typedIfTExprExpr typedElseExpr, tIfTExpr, env)
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
typeExpr env (Parser.FunDecl name args expr) =
  (FunDeclTExpr funVar typedArgs typedExpr, TVoid, addToEnv env funVar)
  where
    typedArgs = map (\(n, t) -> Variable t n) args
    envWithArgs = foldl addToEnv env typedArgs
    argTypesList = map snd args

    unknownFunType = TFun argTypesList TUnknown
    unknownFunVar = Variable unknownFunType name
    envWithUnknown = addToEnv envWithArgs unknownFunVar
    (typedExpr, returnType, _) = typeExpr envWithUnknown expr

    funType = TFun argTypesList returnType
    funVar = Variable funType name
typeExpr env (Parser.FunCall name args) =
  if argTypesList == lookUpArgTypes
    then (FunCallTExpr funVar typedArgs, returnType, env)
    else error $ "Expected arguments " ++ show lookUpArgTypes ++ " for function " ++ name ++ ", got " ++ show argTypesList
  where
    typedArgs = map (\arg -> let (texpr, t, _) = typeExpr env arg in texpr) args
    argTypesList = map (\arg -> let (_, t, _) = typeExpr env arg in t) args
    funType = TFun argTypesList returnType
    funVar = Variable funType name
    (lookUpArgTypes, returnType) =
      case getVarType env name of
        Nothing -> error $ "Function " ++ name ++ " not found in environment"
        Just (TFun argTypes t) -> (argTypes, t)
        Just t -> error $ "Expected " ++ name ++ " to be of type TFun, got " ++ show t
typeExpr env expr = error $ "Type inference not implemented for " ++ show expr

throwIfUnknown :: (HasCallStack) => Type -> a -> a
throwIfUnknown t x = if t == TUnknown then error ("Type inference failed: unknown type\n" ++ prettyCallStack callStack) else x

getType :: TypedExpr -> Type
getType (IntTExpr _) = TInt
getType (BoolTExpr _) = TBool
getType (IdentTExpr (Variable t _)) = t
getType (LetTExpr _ _) = TVoid
getType (IfTExpr t _ _ _) = t
getType (BinOpTExpr t _ _ _) = t
getType (FunDeclTExpr _ _ _) = TVoid
getType (FunCallTExpr (Variable (TFun _ t) _) _) = t
getType (FunCallTExpr (Variable t _) _) = error $ "Expected TFun, got " ++ show t

checkForUnknowns :: (HasCallStack) => TypeEnv -> TypedExpr -> TypedExpr
checkForUnknowns _ (IntTExpr i) = IntTExpr i
checkForUnknowns _ (BoolTExpr b) = BoolTExpr b
checkForUnknowns env (IdentTExpr (Variable t name)) = throwIfUnknown t (IdentTExpr (Variable t name))
checkForUnknowns env (LetTExpr var@(Variable t name) expr) =
  let expr' = checkForUnknowns env expr
      t' = if t == TUnknown then getType expr' else t
      var' = Variable t' name
   in throwIfUnknown TVoid (LetTExpr var' expr')
checkForUnknowns env (IfTExpr t boolExpr ifExpr elseExpr) =
  let boolExpr' = checkForUnknowns env boolExpr
      ifExpr' = checkForUnknowns env ifExpr
      elseExpr' = checkForUnknowns env elseExpr
      t' =
        if t == TUnknown
          then
            let tIf = getType ifExpr'
             in if tIf == TUnknown then getType elseExpr' else tIf
          else t
   in throwIfUnknown t' (IfTExpr t' boolExpr' ifExpr' elseExpr')
checkForUnknowns env (BinOpTExpr t left op right) =
  let left' = checkForUnknowns env left
      right' = checkForUnknowns env right
      t' = if t == TUnknown then checkBinOpType (getType left') op (getType right') else t
   in throwIfUnknown t' (BinOpTExpr t' left' op right')
checkForUnknowns env (FunDeclTExpr var@(Variable (TFun targs returnType) name) args expr) =
  let expr' = checkForUnknowns env expr
      returnType' = if returnType == TUnknown then getType expr' else returnType
      var' = Variable (TFun targs returnType') name
   in throwIfUnknown returnType' (FunDeclTExpr var' args expr')
checkForUnknowns env (FunDeclTExpr {}) = error "Internal compiler error: FunDeclTExpr should have TFun type"
checkForUnknowns env (FunCallTExpr (Variable (TFun _ TUnknown) name) args) =
  case getVarType env name of
    Nothing -> error $ "Function " ++ name ++ " not found in environment, type inference failed"
    Just t -> FunCallTExpr (Variable t name) (map (checkForUnknowns env) args)
checkForUnknowns env (FunCallTExpr var args) = FunCallTExpr var (map (checkForUnknowns env) args)