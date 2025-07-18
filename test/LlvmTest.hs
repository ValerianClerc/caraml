{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module LlvmTest where

import Common (Type (..))
import Compile (compileAndRun)
import Parser (Op (..))
import Test.Hspec
import TestCases (TestCase)
import TestUtils (fullFileTestCases, matchLlvmTestCase)
import ToLlvm (printLlvm, toLLVM)
import TypeInfer (Identifier (..), TypedExpr (..))

llvmSpec :: [TestCase] -> Spec
llvmSpec testCases =
  describe "LLVM tests" $ do
    describe "E2E test cases" $
      fullFileTestCases matchLlvmTestCase
    describe "LLVM compiler tests" $
      do
        describe "Binary operators" $ do
          it "evaluates simple addition" $ do
            let testCase = [BinOpTExpr TInt (IntTExpr 10) OpPlus (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "15"
          it "evaluates simple subtraction" $ do
            let testCase = [BinOpTExpr TInt (IntTExpr 10) OpMinus (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "5"
          it "evaluates simple multiplication" $ do
            let testCase = [BinOpTExpr TInt (IntTExpr 10) OpMult (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "50"
          it "evaluates simple division" $ do
            let testCase = [BinOpTExpr TInt (IntTExpr 10) OpDiv (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "2"
          it "evaluates simple equality" $ do
            let testCase = [BinOpTExpr TBool (IntTExpr 10) OpEq (IntTExpr 10)]
            compileAndRun testCase `shouldReturn` Just "true"
          it "evaluates simple inequality" $ do
            let testCase = [BinOpTExpr TBool (IntTExpr 10) OpNeq (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "true"
          it "evaluates simple less than" $ do
            let testCase = [BinOpTExpr TBool (IntTExpr 10) OpLt (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "false"
          it "evaluates simple greater than" $ do
            let testCase = [BinOpTExpr TBool (IntTExpr 10) OpGt (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "true"
          it "evaluates simple less than or equal" $ do
            let testCase = [BinOpTExpr TBool (IntTExpr 10) OpLeq (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "false"
          it "evaluates simple greater than or equal" $ do
            let testCase = [BinOpTExpr TBool (IntTExpr 10) OpGeq (IntTExpr 5)]
            compileAndRun testCase `shouldReturn` Just "true"
          it "evaluates boolean logic (and)" $ do
            let testCase = [BinOpTExpr TBool (BoolTExpr True) OpAnd (BoolTExpr False)]
            compileAndRun testCase `shouldReturn` Just "false"
          it "evaluates boolean logic (or)" $ do
            let testCase = [BinOpTExpr TBool (BoolTExpr True) OpOr (BoolTExpr False)]
            compileAndRun testCase `shouldReturn` Just "true"
          it "evaluates unary negative" $ do
            let testCase = [BinOpTExpr TInt (IntTExpr 0) OpMinus (IntTExpr 10)]
            compileAndRun testCase `shouldReturn` Just "-10"
        -- TODO: support NOT
        -- it "evaluates boolean logic (not)" $ do
        describe "Variable assignments" $ do
          it "evaluates a simple variable" $ do
            let var = Variable TInt "x"
            let testCase =
                  [ LetTExpr var (IntTExpr 42),
                    IdentTExpr var
                  ]
            compileAndRun testCase `shouldReturn` Just "42"
          it "evaluates a simple boolean variable" $ do
            let var = Variable TBool "y"
            let testCase =
                  [ LetTExpr var (BoolTExpr True),
                    IdentTExpr var
                  ]
            compileAndRun testCase `shouldReturn` Just "true"
          it "evaluates a simple boolean variable (false)" $ do
            let var = Variable TBool "y"
            let testCase =
                  [ LetTExpr var (BoolTExpr False),
                    IdentTExpr var
                  ]
            compileAndRun testCase `shouldReturn` Just "false"
          it "evaluates a simple integer variable" $ do
            let var = Variable TInt "x"
            let testCase =
                  [ LetTExpr var (IntTExpr 99),
                    IdentTExpr var
                  ]
            compileAndRun testCase `shouldReturn` Just "99"
          it "evaluates a simple integer variable (zero)" $ do
            let var = Variable TInt "x"
            let testCase =
                  [ LetTExpr var (IntTExpr 0),
                    IdentTExpr var
                  ]
            compileAndRun testCase `shouldReturn` Just "0"
          it "evaluates a simple integer variable (negative)" $ do
            let var = Variable TInt "x"
            let testCase =
                  [ LetTExpr var (IntTExpr (-42)),
                    IdentTExpr var
                  ]
            compileAndRun testCase `shouldReturn` Just "-42"
          it "evaluates variable reassignment" $ do
            let var = Variable TInt "x"
            let testCase =
                  [ LetTExpr var (IntTExpr 10),
                    LetTExpr var (IntTExpr 20),
                    IdentTExpr var
                  ]
            compileAndRun testCase `shouldReturn` Just "20"
        describe "If/else expressions" $ do
          it "evaluates an if expression (true branch)" $ do
            let testCase = [IfTExpr TInt (BoolTExpr True) (IntTExpr 100) (IntTExpr 200)]
            compileAndRun testCase `shouldReturn` Just "100"

          it "evaluates an if expression (false branch)" $ do
            let testCase = [IfTExpr TInt (BoolTExpr False) (IntTExpr 100) (IntTExpr 200)]
            compileAndRun testCase `shouldReturn` Just "200"

          it "evaluates an if expression with a variable condition" $ do
            let condVar = Variable TBool "cond"
            let testCase =
                  [ LetTExpr condVar (BinOpTExpr TBool (IntTExpr 5) OpEq (IntTExpr 5)), -- cond = true
                    IfTExpr TInt (IdentTExpr condVar) (IntTExpr 1) (IntTExpr 0)
                  ]
            compileAndRun testCase `shouldReturn` Just "1"
        describe "Function calls" $ do
          it "evaluates a simple function call" $ do
            let funVar = Variable (TFun [TInt] TInt) "addTwo"
            let paramVar = Variable TInt "n"
            let testCase =
                  [ FunDeclTExpr funVar [paramVar] (BinOpTExpr TInt (IdentTExpr paramVar) OpPlus (IntTExpr 2)),
                    FunCallTExpr funVar [IntTExpr 10]
                  ]
            compileAndRun testCase `shouldReturn` Just "12"
          it "evaluates a function call with a variable argument" $ do
            let funVar = Variable (TFun [TInt] TInt) "addTwo"
            let paramVar = Variable TInt "n"
            let testCase =
                  [ FunDeclTExpr funVar [paramVar] (BinOpTExpr TInt (IdentTExpr paramVar) OpPlus (IntTExpr 2)),
                    LetTExpr paramVar (IntTExpr 10),
                    FunCallTExpr funVar [IdentTExpr paramVar]
                  ]
            compileAndRun testCase `shouldReturn` Just "12"
          it "evaluates a function call with a binary operation as an argument" $ do
            let funVar = Variable (TFun [TInt] TInt) "addTwo"
            let paramVar = Variable TInt "n"
            let testCase =
                  [ FunDeclTExpr funVar [paramVar] (BinOpTExpr TInt (IdentTExpr paramVar) OpPlus (IntTExpr 2)),
                    FunCallTExpr funVar [BinOpTExpr TInt (IntTExpr 10) OpPlus (IntTExpr 5)]
                  ]
            compileAndRun testCase `shouldReturn` Just "17"
          it "evaluates a function call with a nested function call as an argument" $ do
            let funVar = Variable (TFun [TInt] TInt) "addTwo"
            let paramVar = Variable TInt "n"
            let testCase =
                  [ FunDeclTExpr funVar [paramVar] (BinOpTExpr TInt (IdentTExpr paramVar) OpPlus (IntTExpr 2)),
                    FunCallTExpr funVar [FunCallTExpr funVar [IntTExpr 10]]
                  ]
            compileAndRun testCase `shouldReturn` Just "14"
          it "evaluates a recursive function call" $ do
            let funVar = Variable (TFun [TInt] TInt) "factorial"
            let paramVar = Variable TInt "n"
            let testCase =
                  [ FunDeclTExpr funVar [paramVar] (IfTExpr TInt (BinOpTExpr TBool (IdentTExpr paramVar) OpEq (IntTExpr 0)) (IntTExpr 1) (BinOpTExpr TInt (IdentTExpr paramVar) OpMult (FunCallTExpr funVar [BinOpTExpr TInt (IdentTExpr paramVar) OpMinus (IntTExpr 1)]))),
                    FunCallTExpr funVar [IntTExpr 5]
                  ]
            compileAndRun testCase `shouldReturn` Just "120"

-- TODO: support HOF
-- it "evaluates a higher order function" $ do
--   let funVar = Variable (TFun [TFun [TInt] TInt] TInt) "applyFunc"
--   let paramVar = Variable (TFun [TInt] TInt) "f"
--   let testCase =
--         [ FunDeclTExpr funVar [paramVar] (FunCallTExpr paramVar [IntTExpr 10]),
--           FunCallTExpr funVar [FunDeclTExpr (Variable (TFun [TInt] TInt) "addTwo") [Variable TInt "x"] (BinOpTExpr TInt (IdentTExpr (Variable TInt "x")) OpPlus (IntTExpr 2))]
--         ]
--   compileAndRun testCase `shouldReturn` Just 12