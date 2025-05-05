{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ToLlvm where

import Common (Type (..))
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T
import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.IntegerPredicate
import LLVM.AST.Operand
import LLVM.AST.Type
import LLVM.IRBuilder
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Pretty
import Parser (Op (..))
import TypeInfer

-- Symbol table to store variable name to LLVM operand mappings
type SymbolTable = Map.Map String Operand

printLlvm :: LLVM.AST.Module -> String
printLlvm = T.unpack . ppllvm

printLlvmExpr :: LLVM.AST.Operand -> String
printLlvmExpr = T.unpack . ppll

toLLVM :: [TypedExpr] -> LLVM.AST.Module
toLLVM exprs =
  LLVM.IRBuilder.Module.buildModule "program" $ do
    -- Inject the printint function (To be implemented in C)
    printInt <- LLVM.IRBuilder.extern "printint" [LLVM.AST.Type.i32] LLVM.AST.Type.i32
    printBool <- LLVM.IRBuilder.extern "printbool" [LLVM.AST.Type.i1] LLVM.AST.Type.i32

    -- Define the main function that will evaluate our expressions
    LLVM.IRBuilder.Module.function "main" [] LLVM.AST.Type.i32 $ \[] -> do
      -- Start with an empty symbol table
      let symTable = Map.empty

      -- Evaluate each expression in sequence, threading the symbol table through
      (symTable', results) <-
        foldM
          ( \(st, rs) expr -> do
              (st', r) <- exprToLLVM st expr
              return (st', rs ++ [r])
          )
          (symTable, [])
          exprs

      -- Print the last result (or 0 if empty)
      let lastResult = if Prelude.null results then intToLLVM 0 else Prelude.last results
      -- Print the result
      _ <- LLVM.IRBuilder.Instruction.call printInt [(lastResult, [])]

      -- return 0
      LLVM.IRBuilder.Instruction.ret (LLVM.IRBuilder.Constant.int32 0)

intToLLVM :: Int -> LLVM.AST.Operand.Operand
intToLLVM i = LLVM.AST.Operand.ConstantOperand $ LLVM.AST.Constant.Int 32 (fromIntegral i)

boolToLLVM :: Bool -> LLVM.AST.Operand.Operand
boolToLLVM b = LLVM.AST.Operand.ConstantOperand $ LLVM.AST.Constant.Int 1 (if b then 1 else 0)

binopToLLVM :: Op -> LLVM.AST.Operand.Operand -> LLVM.AST.Operand.Operand -> LLVM.IRBuilder.Monad.MonadIRBuilder m => m LLVM.AST.Operand.Operand
binopToLLVM op lhs rhs = do
  case op of
    OpPlus -> LLVM.IRBuilder.Instruction.add lhs rhs
    OpMinus -> LLVM.IRBuilder.Instruction.sub lhs rhs
    OpMult -> LLVM.IRBuilder.Instruction.mul lhs rhs
    OpDiv -> LLVM.IRBuilder.Instruction.sdiv lhs rhs
    OpLt -> icmp LLVM.AST.IntegerPredicate.SLT lhs rhs
    OpGt -> icmp LLVM.AST.IntegerPredicate.SGT lhs rhs
    OpLeq -> icmp LLVM.AST.IntegerPredicate.SLE lhs rhs
    OpGeq -> icmp LLVM.AST.IntegerPredicate.SGE lhs rhs
    OpEq -> icmp LLVM.AST.IntegerPredicate.EQ lhs rhs
    OpOr -> LLVM.IRBuilder.Instruction.or lhs rhs
    OpAnd -> LLVM.IRBuilder.Instruction.and lhs rhs
    OpNeq -> icmp LLVM.AST.IntegerPredicate.NE lhs rhs
    _ -> error "Unsupported binary operator"

typeToLLVM :: Common.Type -> LLVM.AST.Type
typeToLLVM TInt = LLVM.AST.Type.i32
typeToLLVM TBool = LLVM.AST.Type.i1
typeToLLVM TVoid = LLVM.AST.Type.void
typeToLLVM (TFun args ret) =
  LLVM.AST.Type.FunctionType
    (typeToLLVM ret)
    (Prelude.map typeToLLVM args)
    False
typeToLLVM TUnknown = error "Cannot convert unknown type to LLVM"

letToLLVM ::
  ( LLVM.IRBuilder.Monad.MonadIRBuilder m,
    LLVM.IRBuilder.Module.MonadModuleBuilder m
  ) =>
  SymbolTable ->
  TypeInfer.Identifier ->
  TypedExpr ->
  m (SymbolTable, LLVM.AST.Operand.Operand)
letToLLVM symTable (TypeInfer.Variable t name) expr = do
  (symTable', exprVal) <- exprToLLVM symTable expr
  let updatedSymTable = Map.insert name exprVal symTable'
  pure (updatedSymTable, exprVal)

ifToLLVM ::
  ( LLVM.IRBuilder.Monad.MonadIRBuilder m,
    LLVM.IRBuilder.Module.MonadModuleBuilder m
  ) =>
  SymbolTable ->
  Common.Type ->
  TypedExpr ->
  TypedExpr ->
  TypedExpr ->
  m (SymbolTable, LLVM.AST.Operand.Operand)
ifToLLVM symTable resultType condExpr thenExpr elseExpr = do
  (symTable', condVal) <- exprToLLVM symTable condExpr

  thenBlock <- freshName "then"
  elseBlock <- freshName "else"
  continueBlock <- freshName "continue"

  condBr condVal thenBlock elseBlock

  emitBlockStart thenBlock
  (symTableThen, thenVal) <- exprToLLVM symTable' thenExpr
  thenTerminator <- currentBlock
  br continueBlock

  emitBlockStart elseBlock
  (symTableElse, elseVal) <- exprToLLVM symTable' elseExpr
  elseTerminator <- currentBlock
  br continueBlock

  emitBlockStart continueBlock
  result <- phi [(thenVal, thenTerminator), (elseVal, elseTerminator)]

  pure (symTable', result)

funDeclToLLVM ::
  ( LLVM.IRBuilder.Monad.MonadIRBuilder m,
    LLVM.IRBuilder.Module.MonadModuleBuilder m
  ) =>
  SymbolTable ->
  TypeInfer.Identifier ->
  [TypeInfer.Identifier] ->
  TypedExpr ->
  m (SymbolTable, LLVM.AST.Operand.Operand)
funDeclToLLVM symTable (TypeInfer.Variable (TFun argTypes retType) fnName) args funBody = do
  let llvmArgTypes = Prelude.map (typeToLLVM . TypeInfer.varType) args
      llvmRetType = typeToLLVM retType

  LLVM.IRBuilder.Module.function (fromString fnName) (Prelude.zip llvmArgTypes (Prelude.map (fromString . TypeInfer.name) args)) llvmRetType $ \argOperands -> do
    let paramSymTable = Map.fromList (Prelude.zip (Prelude.map TypeInfer.name args) argOperands)
        mergedSymTable = Map.union paramSymTable symTable

    (_, resultVal) <- exprToLLVM mergedSymTable funBody

    ret resultVal

  pure (symTable, intToLLVM 0) -- Function declaration itself doesn't produce a value in the current scope
funDeclToLLVM _ (TypeInfer.Variable otherType fnName) _ _ =
  error $ "Type error during LLVM generation: Expected function type for declaration of '" ++ fnName ++ "', but got " ++ show otherType

funCallToLLVM ::
  ( LLVM.IRBuilder.Monad.MonadIRBuilder m,
    LLVM.IRBuilder.Module.MonadModuleBuilder m
  ) =>
  SymbolTable ->
  TypeInfer.Identifier ->
  [TypedExpr] ->
  m (SymbolTable, LLVM.AST.Operand.Operand)
funCallToLLVM symTable (TypeInfer.Variable (TFun _ retType) fnName) args = do
  (symTable', argVals) <-
    foldM
      ( \(st, vals) expr -> do
          (st', val) <- exprToLLVM st expr
          pure (st', vals ++ [val])
      )
      (symTable, [])
      args

  result <-
    call
      ( ConstantOperand $
          GlobalReference
            (ptr (FunctionType (typeToLLVM retType) (Prelude.map operandType argVals) False))
            (fromString fnName)
      )
      [(val, []) | val <- argVals]

  pure (symTable', result)
  where
    operandType (LocalReference t _) = t
    operandType (ConstantOperand (Int 32 _)) = i32
    operandType (ConstantOperand (Int 1 _)) = i1
    operandType other = error $ "Unable to determine operand type for " ++ show other ++ " in function call to " ++ fnName
funCallToLLVM _ (TypeInfer.Variable otherType fnName) _ =
  error $ "Type error during LLVM generation: Expected function type for call to '" ++ fnName ++ "', but got " ++ show otherType

exprToLLVM ::
  ( LLVM.IRBuilder.Monad.MonadIRBuilder m,
    LLVM.IRBuilder.Module.MonadModuleBuilder m
  ) =>
  SymbolTable ->
  TypedExpr ->
  m (SymbolTable, LLVM.AST.Operand.Operand)
exprToLLVM symTable (IntTExpr int) =
  pure (symTable, intToLLVM int)
exprToLLVM symTable (BoolTExpr bool) =
  pure (symTable, boolToLLVM bool)
exprToLLVM symTable (BinOpTExpr _ e1 op e2) = do
  (symTable', lhs) <- exprToLLVM symTable e1
  (symTable'', rhs) <- exprToLLVM symTable' e2
  result <- binopToLLVM op lhs rhs
  pure (symTable'', result)
exprToLLVM symTable (IdentTExpr var@(TypeInfer.Variable t name)) =
  case Map.lookup name symTable of
    Just operand -> pure (symTable, operand)
    Nothing -> error $ "Variable not found in symbol table: " ++ name
exprToLLVM symTable (LetTExpr var expr) =
  letToLLVM symTable var expr
exprToLLVM symTable (IfTExpr resultType condExpr thenExpr elseExpr) =
  ifToLLVM symTable resultType condExpr thenExpr elseExpr
exprToLLVM symTable (FunDeclTExpr var args funBody) =
  funDeclToLLVM symTable var args funBody
exprToLLVM symTable (FunCallTExpr var args) =
  funCallToLLVM symTable var args