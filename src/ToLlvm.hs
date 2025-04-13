{-# LANGUAGE OverloadedStrings #-}

module ToLlvm where

import LLVM.AST
import LLVM.AST.Constant
import LLVM.AST.Global
import LLVM.AST.Operand
import LLVM.AST.Type
import LLVM.IRBuilder
import LLVM.IRBuilder.Constant
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.Pretty
import TypeInfer
import Data.Text.Lazy as T
import Data.Text.Lazy.IO as T

printLlvm :: LLVM.AST.Module -> T.Text
printLlvm m = ppllvm m 

-- | given our `TypedExpr` type, turn it into an LLVM module
toLLVM :: [TypedExpr] -> LLVM.AST.Module
toLLVM exprs =
  LLVM.IRBuilder.Module.buildModule "program" $ do
    -- Define the main function that will evaluate our expressions
    LLVM.IRBuilder.Module.function "main" [] LLVM.AST.Type.i32 $ \[] -> do
      -- Evaluate each expression in sequence
      results <- mapM exprToLLVM exprs

      -- Return the last result (or 0 if empty)
      LLVM.IRBuilder.Instruction.ret $
        if Prelude.null results
          then LLVM.AST.Operand.ConstantOperand (LLVM.AST.Constant.Int 32 0)
          else Prelude.last results

intToLLVM :: Int -> LLVM.AST.Operand.Operand
intToLLVM i = LLVM.AST.Operand.ConstantOperand $ LLVM.AST.Constant.Int 32 (fromIntegral i)

exprToLLVM ::
  ( LLVM.IRBuilder.Monad.MonadIRBuilder m,
    LLVM.IRBuilder.Module.MonadModuleBuilder m
  ) =>
  TypedExpr ->
  m LLVM.AST.Operand.Operand
exprToLLVM (IntTExpr int) = pure $ intToLLVM int
exprToLLVM _ = error "LLVM not implemented"