module TypeTest where

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
        it "#1 (already in scope)" $ evaluate (typeExpr intTEnv (Let "x" (LInt 3))) `shouldThrow` anyErrorCall
        it "#2 (same name different type)" $ evaluate (typeExpr intTEnv (Let "x" (LBool True))) `shouldThrow` anyErrorCall
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
  describe "Type inference full file test cases:" $ do
    fullFileTestCases matchTypeInferenceTestCase

-- describe "function declaration" $ do
--   describe "valid" $ do
--   describe "invalid" $ do
-- describe "function application" $ do
--   describe "valid" $ do
--   describe "invalid" $ do
-- describe "binary operation" $ do
--   describe "valid" $ do
--   describe "invalid" $ do