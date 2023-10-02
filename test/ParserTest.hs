module ParserTest where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Lexer
import Lib (Selection (Parser))
import Parser
import Test.Hspec
import TestCases
import TestUtils (fullFileTestCases, matchParseTestCase)

parserTests :: [TestCase] -> Spec
parserTests testCases = do
  describe "Parser unit tests:" $ do
    describe "literals" $ do
      it "digit" $ parseExpr [DIGIT 2001] `shouldBe` (LInt 2001, [])
      it "char" $ parseExpr [CHAR 'c'] `shouldBe` (LChar 'c', [])
      it "string" $ parseExpr [STRING "testString"] `shouldBe` (LString "testString", [])
      it "boolean" $ parseExpr [BOOLEAN True] `shouldBe` (LBool True, [])
    describe "if-then-else" $ do
      describe "valid if" $ do
        it "#1" $ parseExpr [IF, BOOLEAN True, THEN, IDENT "x", ELSE, IDENT "y"] `shouldBe` (Conditional (LBool True) (VarExpr "x") (VarExpr "y"), [])
        it "#2" $ parseExpr [IF, IDENT "x", LST, DIGIT 0, THEN, IDENT "x", ELSE, IDENT "y"] `shouldBe` (Conditional (BinOp (VarExpr "x") OpLt (LInt 0)) (VarExpr "x") (VarExpr "y"), [])
      describe "invalid if" $ do
        it "#1 (missing THEN)" $ evaluate (parseExpr [IF, IDENT "x", LST, DIGIT 0, IDENT "x", ELSE, IDENT "y"]) `shouldThrow` anyErrorCall
        it "#2 (invalid predicate)" $ evaluate (parseExpr [IF, EQU, THEN, IDENT "x", ELSE, IDENT "y"]) `shouldThrow` anyErrorCall
        it "#3 (invalid else)" $ evaluate (parseExpr [IF, BOOLEAN True, THEN, IDENT "x", ELSE, FUN]) `shouldThrow` anyErrorCall
    describe "variable declaration" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [LET, IDENT "x", EQU, DIGIT 3] `shouldBe` (Let "x" (LInt 3), [])
      describe "invalid" $ do
        it "#1 (missing variable identifier)" $ evaluate (parseExpr [LET, EQU, DIGIT 3]) `shouldThrow` anyErrorCall
        it "#2 (extra digit after identifier)" $ evaluate (parseExpr [LET, IDENT "x", DIGIT 3, EQU, BOOLEAN True]) `shouldThrow` anyErrorCall
    describe "function declaration" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COMMA, IDENT "p2", RPAREN, EQU, IDENT "p1"] `shouldBe` (FunDecl "x" ["p1", "p2"] (VarExpr "p1"), [])
      describe "invalid" $ do
        it "#1 (missing right parens)" $ evaluate (parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COMMA, IDENT "p2", EQU, IDENT "p1"]) `shouldThrow` anyErrorCall
        it "#2 (extra digit after identifier)" $ evaluate (parseExpr [FUN, IDENT "x", DIGIT 3, LPAREN, IDENT "p1", COMMA, IDENT "p2", RPAREN, EQU, IDENT "p1"]) `shouldThrow` anyErrorCall
        it "#3 (missing comma)" $ evaluate (force (parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", IDENT "p2", RPAREN, EQU, IDENT "p1"])) `shouldThrow` anyErrorCall
    describe "function application" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [IDENT "x", LPAREN, DIGIT 3, COMMA, IDENT "p2", RPAREN] `shouldBe` (FunCall "x" [LInt 3, VarExpr "p2"], [])
      describe "invalid" $ do
        it "#1 (missing right parens)" $ evaluate (parseExpr [IDENT "x", LPAREN, DIGIT 3, IDENT "p2"]) `shouldThrow` anyErrorCall
        it "#2 (no comma)" $ evaluate (parseExpr [IDENT "x", LPAREN, DIGIT 3, IDENT "p2"]) `shouldThrow` anyErrorCall
    describe "binary operation" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [IDENT "x", PLUS, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpPlus (LInt 3), [])
      describe "invalid" $ do
        it "#1 (missing LHS)" $ evaluate (parseExpr [PLUS, DIGIT 3]) `shouldThrow` anyErrorCall
        it "#2 (invalid RHS)" $ evaluate (force (parseExpr [IDENT "x", PLUS, FUN, DIGIT 3])) `shouldThrow` anyErrorCall
        it "#3 (missing RHS)" $ evaluate (force (parseExpr [IDENT "x", PLUS])) `shouldThrow` anyErrorCall
  describe "Parser full file test cases:" $ do
    fullFileTestCases matchParseTestCase