{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module ToLlvm where

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

printLlvm :: LLVM.AST.Module -> String
printLlvm = T.unpack . ppllvm

printLlvmExpr :: LLVM.AST.Operand -> String
printLlvmExpr = T.unpack . ppll

-- | given our `TypedExpr` type, turn it into an LLVM module
toLLVM :: [TypedExpr] -> LLVM.AST.Module
toLLVM exprs =
  LLVM.IRBuilder.Module.buildModule "program" $ do
    -- Inject the printint function (To be implemented in C)
    printInt <- LLVM.IRBuilder.extern "printint" [LLVM.AST.Type.i32] LLVM.AST.Type.i32

    -- Define the main function that will evaluate our expressions
    LLVM.IRBuilder.Module.function "main" [] LLVM.AST.Type.i32 $ \[] -> do
      -- Evaluate each expression in sequence
      results <- mapM exprToLLVM exprs

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
    OpNeq -> icmp LLVM.AST.IntegerPredicate.NE lhs rhs
    _ -> error "Unsupported binary operator"

exprToLLVM ::
  ( LLVM.IRBuilder.Monad.MonadIRBuilder m,
    LLVM.IRBuilder.Module.MonadModuleBuilder m
  ) =>
  TypedExpr ->
  m LLVM.AST.Operand.Operand
exprToLLVM (IntTExpr int) = pure $ intToLLVM int
exprToLLVM (BoolTExpr bool) = pure $ boolToLLVM bool
exprToLLVM (BinOpTExpr _ e1 op e2) = do
  lhs <- exprToLLVM e1
  rhs <- exprToLLVM e2
  binopToLLVM op lhs rhs
exprToLLVM _ = error "LLVM not implemented for this expression type"