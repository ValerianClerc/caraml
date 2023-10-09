module TypeTest where

import Common (Type (..))
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Parser
import Test.Hspec
import TestCases
import TestUtils (fullFileTestCases, matchTypeInferenceTestCase)
import TypeInfer

emptyTEnv = TypeEnv []

-- int x
intTEnv = TypeEnv [Variable TInt "x"]

-- int x, int z
twoIntTEnv = TypeEnv [Variable TInt "x", Variable TInt "z"]

-- bool y
boolTEnv = TypeEnv [Variable TBool "y"]

-- int x, bool y
bothTEnv = TypeEnv [Variable TInt "x", Variable TBool "y"]

bothTEnvReverse = TypeEnv $ reverse xs
  where
    TypeEnv xs = bothTEnv

-- int x, bool y, int z
fullEnv = TypeEnv [Variable TInt "x", Variable TBool "y", Variable TInt "z"]

typeTests :: [TestCase] -> Spec
typeTests testCases = do
  describe "Type inference unit tests:" $ do
    describe "literals" $ do
      it "digit" $ typeExpr emptyTEnv (LInt 2001) `shouldBe` (IntTExpr 2001, TInt, TypeEnv [])
      it "bool" $ typeExpr emptyTEnv (LBool True) `shouldBe` (BoolTExpr True, TBool, TypeEnv [])
    describe "variable declaration" $ do
      describe "valid" $ do
        it "#1 (new int, empty env)" $ typeExpr emptyTEnv (Let "x" (LInt 3)) `shouldBe` (LetTExpr (Variable TInt "x") (IntTExpr 3), TVoid, intTEnv)
        it "#2 (new bool, int already in env)" $ typeExpr intTEnv (Let "y" (LBool True)) `shouldBe` (LetTExpr (Variable TBool "y") (BoolTExpr True), TVoid, bothTEnvReverse)
        it "#3 (new int, int already in env)" $ typeExpr intTEnv (Let "y" (LInt 3)) `shouldBe` (LetTExpr (Variable TInt "y") (IntTExpr 3), TVoid, TypeEnv [Variable TInt "y", Variable TInt "x"])
      describe "invalid" $ do
        it "#1 (already in scope)" $ evaluate (force (typeExpr intTEnv (Let "x" (LInt 3)))) `shouldThrow` anyErrorCall
        it "#2 (same name different type)" $ evaluate (force (typeExpr intTEnv (Let "x" (LBool True)))) `shouldThrow` anyErrorCall
    describe "variable expression" $ do
      describe "valid" $ do
        it "#1 (int)" $ typeExpr intTEnv (VarExpr "x") `shouldBe` (IdentTExpr (Variable TInt "x"), TInt, intTEnv)
        it "#2 (bool)" $ typeExpr boolTEnv (VarExpr "y") `shouldBe` (IdentTExpr (Variable TBool "y"), TBool, boolTEnv)
      describe "invalid" $ do
        it "#1 (not in scope)" $ evaluate (typeExpr intTEnv (VarExpr "y")) `shouldThrow` anyErrorCall
        it "#2 (not in scope)" $ evaluate (typeExpr boolTEnv (VarExpr "x")) `shouldThrow` anyErrorCall
    describe "if-then-else" $ do
      describe "valid" $ do
        it "#1 (hardcoded bool condition, both return int)" $ typeExpr twoIntTEnv (Conditional (LBool True) (VarExpr "x") (VarExpr "z")) `shouldBe` (IfTExpr TInt (BoolTExpr True) (IdentTExpr (Variable TInt "x")) (IdentTExpr (Variable TInt "z")), TInt, twoIntTEnv)
        it "#2 (bool variable condition, both return int)" $ typeExpr fullEnv (Conditional (VarExpr "y") (VarExpr "x") (VarExpr "z")) `shouldBe` (IfTExpr TInt (IdentTExpr (Variable TBool "y")) (IdentTExpr (Variable TInt "x")) (IdentTExpr (Variable TInt "z")), TInt, fullEnv)
        it "#3 (bool variable condition, hardcode bool return)" $ typeExpr boolTEnv (Conditional (VarExpr "y") (VarExpr "y") (LBool True)) `shouldBe` (IfTExpr TBool (IdentTExpr (Variable TBool "y")) (IdentTExpr (Variable TBool "y")) (BoolTExpr True), TBool, boolTEnv)
      describe "invalid" $ do
        it "#1 (condition not bool)" $ evaluate (typeExpr intTEnv (Conditional (VarExpr "x") (VarExpr "x") (VarExpr "x"))) `shouldThrow` anyErrorCall
        it "#2 (if and else return different types)" $ evaluate (typeExpr fullEnv (Conditional (LBool True) (VarExpr "x") (VarExpr "y"))) `shouldThrow` anyErrorCall
        it "#3 (condition not in scope)" $ evaluate (typeExpr intTEnv (Conditional (VarExpr "y") (VarExpr "x") (VarExpr "x"))) `shouldThrow` anyErrorCall
    describe "binops" $ do
      describe "valid" $ do
        it "#1 (addition)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpPlus (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpPlus (IntTExpr 2), TInt, emptyTEnv)
        it "#2 (subtraction)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpMinus (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpMinus (IntTExpr 2), TInt, emptyTEnv)
        it "#3 (multiplication)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpMult (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpMult (IntTExpr 2), TInt, emptyTEnv)
        it "#4 (division)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpDiv (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpDiv (IntTExpr 2), TInt, emptyTEnv)
        it "#5 (equality bool)" $ typeExpr emptyTEnv (BinOp (LBool True) OpEq (LBool False)) `shouldBe` (BinOpTExpr TBool (BoolTExpr True) OpEq (BoolTExpr False), TBool, emptyTEnv)
        it "#6 (equality int)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpEq (LInt 2)) `shouldBe` (BinOpTExpr TBool (IntTExpr 1) OpEq (IntTExpr 2), TBool, emptyTEnv)
        it "#7 (less than)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpLt (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpLt (IntTExpr 2), TInt, emptyTEnv)
        it "#8 (less than or equal)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpLeq (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpLeq (IntTExpr 2), TInt, emptyTEnv)
        it "#9 (greater than)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpGt (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpGt (IntTExpr 2), TInt, emptyTEnv)
        it "#10 (greater than or equal)" $ typeExpr emptyTEnv (BinOp (LInt 1) OpGeq (LInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpGeq (IntTExpr 2), TInt, emptyTEnv)
      describe "invalid" $ do
        it "#1 (different typed operands)" $ evaluate (force (typeExpr emptyTEnv (BinOp (LInt 1) OpEq (LBool True)))) `shouldThrow` anyErrorCall
        it "#1 (invalid operands)" $ evaluate (force (typeExpr emptyTEnv (BinOp (LBool False) OpPlus (LBool True)))) `shouldThrow` anyErrorCall
    describe "function declaration" $ do
      describe "valid" $ do
        it "#1 (no args, return int)" $ typeExpr emptyTEnv (FunDecl "x" [] (LInt 1)) `shouldBe` (FunDeclTExpr (Variable (TFun [] TInt) "x") [] (IntTExpr 1), TVoid, TypeEnv [Variable (TFun [] TInt) "x"])
        it "#2 (one int arg, return that int)" $ typeExpr emptyTEnv (FunDecl "x" [("y", TInt)] (VarExpr "y")) `shouldBe` (FunDeclTExpr (Variable (TFun [TInt] TInt) "x") [Variable TInt "y"] (IdentTExpr (Variable TInt "y")), TVoid, TypeEnv [Variable (TFun [TInt] TInt) "x"])
        it "#3 (two int args, return sum)" $ typeExpr emptyTEnv (FunDecl "x" [("y", TInt), ("z", TInt)] (BinOp (VarExpr "y") OpPlus (VarExpr "z"))) `shouldBe` (FunDeclTExpr (Variable (TFun [TInt, TInt] TInt) "x") [Variable TInt "y", Variable TInt "z"] (BinOpTExpr TInt (IdentTExpr (Variable TInt "y")) OpPlus (IdentTExpr (Variable TInt "z"))), TVoid, TypeEnv [Variable (TFun [TInt, TInt] TInt) "x"])
        it "#4 (one bool arg, return that bool)" $ typeExpr emptyTEnv (FunDecl "x" [("y", TBool)] (VarExpr "y")) `shouldBe` (FunDeclTExpr (Variable (TFun [TBool] TBool) "x") [Variable TBool "y"] (IdentTExpr (Variable TBool "y")), TVoid, TypeEnv [Variable (TFun [TBool] TBool) "x"])
        it "#5 (no args, return int from env)" $ typeExpr intTEnv (FunDecl "f" [] (VarExpr "x")) `shouldBe` (FunDeclTExpr (Variable (TFun [] TInt) "f") [] (IdentTExpr (Variable TInt "x")), TVoid, TypeEnv [Variable (TFun [] TInt) "f", Variable TInt "x"])
      describe "invalid" $ do
        it "#1 (already in scope)" $ evaluate (force (typeExpr intTEnv (FunDecl "x" [] (LInt 1)))) `shouldThrow` anyErrorCall
        it "#2 (same name different type)" $ evaluate (force (typeExpr intTEnv (FunDecl "x" [] (LBool True)))) `shouldThrow` anyErrorCall
  describe "Type inference full file test cases:" $ do
    fullFileTestCases matchTypeInferenceTestCase

-- describe "function application" $ do
--   describe "valid" $ do
--   describe "invalid" $ do
-- describe "binary operation" $ do
--   describe "valid" $ do
--   describe "invalid" $ do