module LlvmTest where

import Parser
import Test.Hspec
import TestCases
import ToLlvm
import TypeInfer

-- llvmTests :: [TestCase] -> Spec
-- llvmTests testCases = do
--   describe "LLVM tests" $ do
--     describe "LLVM code generation" $ do
--       it "empty program" $ do
--         let testCase = TestCase {rawTestCase = "", lexedTestCase = [], parsedTestCase = [], typedTestCase = []}
--         llvmCode <- generateLLVM testCase
--         llvmCode `shouldBe` ""
--       it "simple function" $ do
--         let testCase = TestCase {rawTestCase = "let x = 42; x;", lexedTestCase = [LET, IDENT "x", EQU, DIGIT 42, SC, IDENT "x", SC, EOF], parsedTestCase = [Let {Parser.letVar = "x", Parser.letEqual = PInt 42}, VarExpr "x"], typedTestCase = [LetTExpr {TypeInfer.letVar = Variable TInt "x", TypeInfer.letEqual = IntTExpr 42}, IdentTExpr (Variable TInt "x")]}
--         llvmCode <- generateLLVM testCase
--         llvmCode `shouldBe` "define i32 @main() {\nentry:\n  ret i32 42\n}\n"