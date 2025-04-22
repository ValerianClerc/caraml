{-# LANGUAGE BlockArguments #-}

module LlvmTest where

import Common (Type (..))
import qualified Data.Text.Lazy as T
import Test.Hspec
import TestCases
import ToLlvm
import TypeInfer

llvmTests :: [TestCase] -> Spec
llvmTests testCases = do
  describe "LLVM tests" $
    do
      it "placeholder" $ 1 `shouldBe` 1

-- it "let x = 3 in x" $ (printLlvmExpr $ (pure $ (exprToLLVM (LetTExpr (Variable TInt "x") (IntTExpr 3))))) `shouldBe` "x = alloca i32\nstore i32 3, i32* x\nx\n"

--   it "let y = true in y" $ llvmExpr (LetTExpr "y" (BoolTExpr True)) `shouldBe` "y = alloca i1\nstore i1 1, i1* y\ny\n"
--   it "let z = 5 in z" $ llvmExpr (LetTExpr "z" (IntTExpr 5)) `shouldBe` "z = alloca i32\nstore i32 5, i32* z\nz\n"
--   it "let w = false in w" $ llvmExpr (LetTExpr "w" (BoolTExpr False)) `shouldBe` "w = alloca i1\nstore i1 0, i1* w\nw\n"
-- describe
-- "if-then-else"
--   $ do
--     it "if x then y else z" $ llvmExpr (IfTExpr TInt (IdentTExpr "x") (IdentTExpr "y") (IdentTExpr "z")) `shouldBe` "if x:\n  y\nelse:\n  z\n"
--     it "if y then x else z" $ llvmExpr (IfTExpr TBool (IdentTExpr "y") (IdentTExpr "x") (IdentTExpr "z")) `shouldBe` "if y:\n  x\nelse:\n  z\n"
--     it "if x then true else false" $ llvmExpr (IfTExpr TBool (IdentTExpr "x") (BoolTExpr True) (BoolTExpr False)) `shouldBe` "if x:\n  true\nelse:\n  false\n"
--   describe
--   "function declaration"
--     $ do
--       it "fun f (x: int) = x" $ llvmExpr (FunDeclTExpr "f" [("x", TInt)] (IdentTExpr "x")) `shouldBe` "define i32 @f(i32 %x) {\n  ret i32 %x\n}\n"
--       it "fun g (y: bool) = y" $ llvmExpr (FunDeclTExpr "g" [("y", TBool)] (IdentTExpr "y")) `shouldBe` "define i1 @g(i1 %y) {\n  ret i1 %y\n}\n"
--       it "fun h (x: int, y: bool) = if x then y else false" $ llvmExpr (FunDeclTExpr "h" [("x", TInt), ("y", TBool)] (IfTExpr TBool (IdentTExpr "x") (IdentTExpr "y") (BoolTExpr False))) `shouldBe` "define i1 @h(i32 %x, i1 %y) {\n  if %x:\n    %y\n  else:\n    false\n}\n"
--       it "fun i (x: int, y: int) = x + y" $ llvmExpr (FunDeclTExpr "i" [("x", TInt), ("y", TInt)] (BinOpTExpr TInt (IdentTExpr "x") OpPlus (IdentTExpr "y"))) `shouldBe` "define i32 @i(i32 %x, i32 %y) {\n  ret i32 %x + %y\n}\n"
--     describe
--     "function call"
--       $ do
--         it "f(1)" $ llvmExpr (FunCallTExpr "f" [IntTExpr 1]) `shouldBe` "call i32 @f(i32 1)\n"
--         it "g(true)" $ llvmExpr (FunCallTExpr "g" [BoolTExpr True]) `shouldBe` "call i1 @g(i1 1)\n"
--         it "h(x, y)" $ llvmExpr (FunCallTExpr "h" [IdentTExpr "x", IdentTExpr "y"]) `shouldBe` "call i1 @h(i32 %x, i1 %y)\n"
--         it "i(x, y)" $ llvmExpr (FunCallTExpr "i" [IdentTExpr "x", IdentTExpr "y"]) `shouldBe` "call i32 @i(i32 %x, i32 %y)\n"
--       describe
--       "binary operations"
--         $ do
--           it "x + y" $ llvmExpr (BinOpTExpr TInt (IdentTExpr "x") OpPlus (IdentTExpr "y")) `shouldBe` "%x + %y\n"
--           it "x - y" $ llvmExpr (BinOpTExpr TInt (IdentTExpr "x") OpMinus (IdentTExpr "y")) `shouldBe` "%x - %y\n"
--           it "x * y" $ llvmExpr (BinOpTExpr TInt (IdentTExpr "x") OpTimes (IdentTExpr "y")) `shouldBe` "%x * %y\n"
--           it "x / y" $ llvmExpr (BinOpTExpr TInt (IdentTExpr "x") OpDiv (IdentTExpr "y")) `shouldBe` "%x / %y\n"
--           it "x < y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpLt (IdentTExpr "y")) `shouldBe` "%x < %y\n"
--           it "x > y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpGt (IdentTExpr "y")) `shouldBe` "%x > %y\n"
--           it "x <= y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpLeq (IdentTExpr "y")) `shouldBe` "%x <= %y\n"
--           it "x >= y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpGeq (IdentTExpr "y")) `shouldBe` "%x >= %y\n"
--           it "x == y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpEq (IdentTExpr "y")) `shouldBe` "%x == %y\n"
--           it "x <> y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpNeq (IdentTExpr "y")) `shouldBe` "%x != %y\n"
--           it "x && y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpAnd (IdentTExpr "y")) `shouldBe` "%x && %y\n"
--           it "x || y" $ llvmExpr (BinOpTExpr TBool (IdentTExpr "x") OpOr (IdentTExpr "y")) `shouldBe` "%x || %y\n"