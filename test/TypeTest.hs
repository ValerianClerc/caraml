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

funcEnv = TypeEnv [Variable (TFun [TBool] TInt) "f"]

typeTests :: [TestCase] -> Spec
typeTests testCases = do
  describe "Type inference unit tests:" $ do
    describe "literals" $ do
      it "digit" $ typeExpr emptyTEnv (PInt 2001) `shouldBe` (IntTExpr 2001, TInt, TypeEnv [])
      it "bool" $ typeExpr emptyTEnv (PBool True) `shouldBe` (BoolTExpr True, TBool, TypeEnv [])
    describe "variable declaration" $ do
      describe "valid" $ do
        it "#1 (new int, empty env)" $ typeExpr emptyTEnv (Let "x" (PInt 3)) `shouldBe` (LetTExpr (Variable TInt "x") (IntTExpr 3), TVoid, intTEnv)
        it "#2 (new bool, int already in env)" $ typeExpr intTEnv (Let "y" (PBool True)) `shouldBe` (LetTExpr (Variable TBool "y") (BoolTExpr True), TVoid, bothTEnvReverse)
        it "#3 (new int, int already in env)" $ typeExpr intTEnv (Let "y" (PInt 3)) `shouldBe` (LetTExpr (Variable TInt "y") (IntTExpr 3), TVoid, TypeEnv [Variable TInt "y", Variable TInt "x"])
      describe "invalid" $ do
        it "#1 (already in scope)" $ evaluate (force (typeExpr intTEnv (Let "x" (PInt 3)))) `shouldThrow` anyErrorCall
        it "#2 (same name different type)" $ evaluate (force (typeExpr intTEnv (Let "x" (PBool True)))) `shouldThrow` anyErrorCall
    describe "variable expression" $ do
      describe "valid" $ do
        it "#1 (int)" $ typeExpr intTEnv (VarExpr "x") `shouldBe` (IdentTExpr (Variable TInt "x"), TInt, intTEnv)
        it "#2 (bool)" $ typeExpr boolTEnv (VarExpr "y") `shouldBe` (IdentTExpr (Variable TBool "y"), TBool, boolTEnv)
      describe "invalid" $ do
        it "#1 (not in scope)" $ evaluate (typeExpr intTEnv (VarExpr "y")) `shouldThrow` anyErrorCall
        it "#2 (not in scope)" $ evaluate (typeExpr boolTEnv (VarExpr "x")) `shouldThrow` anyErrorCall
    describe "if-then-else" $ do
      describe "valid" $ do
        it "#1 (hardcoded bool condition, both return int)" $ typeExpr twoIntTEnv (Conditional (PBool True) (VarExpr "x") (VarExpr "z")) `shouldBe` (IfTExpr TInt (BoolTExpr True) (IdentTExpr (Variable TInt "x")) (IdentTExpr (Variable TInt "z")), TInt, twoIntTEnv)
        it "#2 (bool variable condition, both return int)" $ typeExpr fullEnv (Conditional (VarExpr "y") (VarExpr "x") (VarExpr "z")) `shouldBe` (IfTExpr TInt (IdentTExpr (Variable TBool "y")) (IdentTExpr (Variable TInt "x")) (IdentTExpr (Variable TInt "z")), TInt, fullEnv)
        it "#3 (bool variable condition, hardcode bool return)" $ typeExpr boolTEnv (Conditional (VarExpr "y") (VarExpr "y") (PBool True)) `shouldBe` (IfTExpr TBool (IdentTExpr (Variable TBool "y")) (IdentTExpr (Variable TBool "y")) (BoolTExpr True), TBool, boolTEnv)
      describe "invalid" $ do
        it "#1 (condition not bool)" $ evaluate (typeExpr intTEnv (Conditional (VarExpr "x") (VarExpr "x") (VarExpr "x"))) `shouldThrow` anyErrorCall
        it "#2 (if and else return different types)" $ evaluate (typeExpr fullEnv (Conditional (PBool True) (VarExpr "x") (VarExpr "y"))) `shouldThrow` anyErrorCall
        it "#3 (condition not in scope)" $ evaluate (typeExpr intTEnv (Conditional (VarExpr "y") (VarExpr "x") (VarExpr "x"))) `shouldThrow` anyErrorCall
    describe "binops" $ do
      describe "valid" $ do
        it "#1 (addition)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpPlus (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpPlus (IntTExpr 2), TInt, emptyTEnv)
        it "#2 (subtraction)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpMinus (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpMinus (IntTExpr 2), TInt, emptyTEnv)
        it "#3 (multiplication)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpMult (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpMult (IntTExpr 2), TInt, emptyTEnv)
        it "#4 (division)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpDiv (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpDiv (IntTExpr 2), TInt, emptyTEnv)
        it "#5 (equality bool)" $ typeExpr emptyTEnv (BinOp (PBool True) OpEq (PBool False)) `shouldBe` (BinOpTExpr TBool (BoolTExpr True) OpEq (BoolTExpr False), TBool, emptyTEnv)
        it "#6 (equality int)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpEq (PInt 2)) `shouldBe` (BinOpTExpr TBool (IntTExpr 1) OpEq (IntTExpr 2), TBool, emptyTEnv)
        it "#7 (less than)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpLt (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpLt (IntTExpr 2), TInt, emptyTEnv)
        it "#8 (less than or equal)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpLeq (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpLeq (IntTExpr 2), TInt, emptyTEnv)
        it "#9 (greater than)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpGt (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpGt (IntTExpr 2), TInt, emptyTEnv)
        it "#10 (greater than or equal)" $ typeExpr emptyTEnv (BinOp (PInt 1) OpGeq (PInt 2)) `shouldBe` (BinOpTExpr TInt (IntTExpr 1) OpGeq (IntTExpr 2), TInt, emptyTEnv)
      describe "invalid" $ do
        it "#1 (different typed operands)" $ evaluate (force (typeExpr emptyTEnv (BinOp (PInt 1) OpEq (PBool True)))) `shouldThrow` anyErrorCall
        it "#1 (invalid operands)" $ evaluate (force (typeExpr emptyTEnv (BinOp (PBool False) OpPlus (PBool True)))) `shouldThrow` anyErrorCall
    describe "function declaration" $ do
      describe "valid" $ do
        it "#1 (no args, return int)" $ typeExpr emptyTEnv (FunDecl "x" [] (PInt 1)) `shouldBe` (FunDeclTExpr (Variable (TFun [] TInt) "x") [] (IntTExpr 1), TVoid, TypeEnv [Variable (TFun [] TInt) "x"])
        it "#2 (one int arg, return that int)" $ typeExpr emptyTEnv (FunDecl "x" [("y", TInt)] (VarExpr "y")) `shouldBe` (FunDeclTExpr (Variable (TFun [TInt] TInt) "x") [Variable TInt "y"] (IdentTExpr (Variable TInt "y")), TVoid, TypeEnv [Variable (TFun [TInt] TInt) "x"])
        it "#3 (two int args, return sum)" $ typeExpr emptyTEnv (FunDecl "x" [("y", TInt), ("z", TInt)] (BinOp (VarExpr "y") OpPlus (VarExpr "z"))) `shouldBe` (FunDeclTExpr (Variable (TFun [TInt, TInt] TInt) "x") [Variable TInt "y", Variable TInt "z"] (BinOpTExpr TInt (IdentTExpr (Variable TInt "y")) OpPlus (IdentTExpr (Variable TInt "z"))), TVoid, TypeEnv [Variable (TFun [TInt, TInt] TInt) "x"])
        it "#4 (one bool arg, return that bool)" $ typeExpr emptyTEnv (FunDecl "x" [("y", TBool)] (VarExpr "y")) `shouldBe` (FunDeclTExpr (Variable (TFun [TBool] TBool) "x") [Variable TBool "y"] (IdentTExpr (Variable TBool "y")), TVoid, TypeEnv [Variable (TFun [TBool] TBool) "x"])
        it "#5 (no args, return int from env)" $ typeExpr intTEnv (FunDecl "f" [] (VarExpr "x")) `shouldBe` (FunDeclTExpr (Variable (TFun [] TInt) "f") [] (IdentTExpr (Variable TInt "x")), TVoid, TypeEnv [Variable (TFun [] TInt) "f", Variable TInt "x"])
      describe "invalid" $ do
        it "#1 (already in scope)" $ evaluate (force (typeExpr intTEnv (FunDecl "x" [] (PInt 1)))) `shouldThrow` anyErrorCall
        it "#2 (same name different type)" $ evaluate (force (typeExpr intTEnv (FunDecl "x" [] (PBool True)))) `shouldThrow` anyErrorCall
        it "#3 (can't infer recursive function type)" $ evaluate (force (runTyper [FunDecl "x" [] (FunCall "x" [])])) `shouldThrow` anyErrorCall
        it "#4 (ambiguous return type)" $ evaluate (force (runTyper [FunDecl "f" [("x", TInt)] (Conditional (BinOp (VarExpr "x") OpEq (PInt 1)) (FunCall "f" [VarExpr "x"]) (FunCall "f" [BinOp (VarExpr "x") OpMinus (PInt 1)]))])) `shouldThrow` anyErrorCall
    describe "function application" $ do
      describe "valid" $ do
        it "#1 (bool arg)" $ typeExpr funcEnv (FunCall "f" [PBool True]) `shouldBe` (FunCallTExpr (Variable (TFun [TBool] TInt) "f") [BoolTExpr True], TInt, funcEnv)
        it "#2 (binop in arg)" $ typeExpr funcEnv (FunCall "f" [BinOp (PInt 1) OpEq (PInt 2)]) `shouldBe` (FunCallTExpr (Variable (TFun [TBool] TInt) "f") [BinOpTExpr TBool (IntTExpr 1) OpEq (IntTExpr 2)], TInt, funcEnv)
        it "#3 (function application in arg)" $ typeExpr funcEnv (FunCall "f" [BinOp (FunCall "f" [PBool True]) OpEq (PInt 1)]) `shouldBe` (FunCallTExpr (Variable (TFun [TBool] TInt) "f") [BinOpTExpr TBool (FunCallTExpr (Variable (TFun [TBool] TInt) "f") [BoolTExpr True]) OpEq (IntTExpr 1)], TInt, funcEnv)
      describe "invalid" $ do
        it "#1 (not in scope)" $ evaluate (force (typeExpr emptyTEnv (FunCall "f" [PBool True]))) `shouldThrow` anyErrorCall
        it "#2 (wrong number of args)" $ evaluate (force (typeExpr funcEnv (FunCall "f" [PBool True, PBool False]))) `shouldThrow` anyErrorCall
        it "#3 (wrong type of args)" $ evaluate (force (typeExpr funcEnv (FunCall "f" [PInt 1]))) `shouldThrow` anyErrorCall
  describe "Type inference full file test cases:" $ do
    fullFileTestCases matchTypeInferenceTestCase
