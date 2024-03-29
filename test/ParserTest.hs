module ParserTest where

import Common (Type (..))
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
        it "#1" $ parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COLON, KBOOL, COMMA, IDENT "p2", COLON, KINT, RPAREN, EQU, IDENT "p1"] `shouldBe` (FunDecl "x" [("p1", TBool), ("p2", TInt)] (VarExpr "p1"), [])
      describe "invalid" $ do
        it "#1 (missing right parens)" $ evaluate (parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COLON, KBOOL, COMMA, IDENT "p2", COLON, KINT, EQU, IDENT "p1"]) `shouldThrow` anyErrorCall
        it "#2 (extra digit after identifier)" $ evaluate (parseExpr [FUN, IDENT "x", DIGIT 3, LPAREN, IDENT "p1", COLON, KBOOL, COMMA, IDENT "p2", COLON, KINT, RPAREN, EQU, IDENT "p1"]) `shouldThrow` anyErrorCall
        it "#3 (missing comma)" $ evaluate (force (parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COLON, KBOOL, IDENT "p2", COLON, KINT, RPAREN, EQU, IDENT "p1"])) `shouldThrow` anyErrorCall
        it "#3 (missing type annotations)" $ evaluate (force (parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COMMA, IDENT "p2", RPAREN, EQU, IDENT "p1"])) `shouldThrow` anyErrorCall
    describe "function application" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [IDENT "x", LPAREN, DIGIT 3, COMMA, IDENT "p2", RPAREN] `shouldBe` (FunCall "x" [LInt 3, VarExpr "p2"], [])
        it "#2 (empty args)" $ parseExpr [IDENT "x", LPAREN, RPAREN] `shouldBe` (FunCall "x" [], [])
      describe "invalid" $ do
        it "#1 (missing right parens)" $ evaluate (parseExpr [IDENT "x", LPAREN, DIGIT 3, IDENT "p2"]) `shouldThrow` anyErrorCall
        it "#2 (no comma)" $ evaluate (parseExpr [IDENT "x", LPAREN, DIGIT 3, IDENT "p2"]) `shouldThrow` anyErrorCall
    describe "binary operation" $ do
      describe "valid" $ do
        it "#1 (plus)" $ parseExpr [IDENT "x", PLUS, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpPlus (LInt 3), [])
        it "#2 (minus)" $ parseExpr [IDENT "x", MINUS, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpMinus (LInt 3), [])
        it "#3 (mult)" $ parseExpr [IDENT "x", ASTERISK, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpMult (LInt 3), [])
        it "#4 (divide)" $ parseExpr [IDENT "x", DIVIDE, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpDiv (LInt 3), [])
        it "#5 (equal)" $ parseExpr [IDENT "x", EQU, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpEq (LInt 3), [])
        it "#6 (greater than)" $ parseExpr [IDENT "x", GRT, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpGt (LInt 3), [])
        it "#7 (greater than or equal)" $ parseExpr [IDENT "x", GEQ, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpGeq (LInt 3), [])
        it "#8 (less than)" $ parseExpr [IDENT "x", LST, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpLt (LInt 3), [])
        it "#9 (less than or equal)" $ parseExpr [IDENT "x", LEQ, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpLeq (LInt 3), [])
        it "#10 (not equal)" $ parseExpr [IDENT "x", NEQ, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpNeq (LInt 3), [])
        it "#11 (nested)" $ parseExpr [IDENT "x", PLUS, IDENT "y", ASTERISK, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpPlus (BinOp (VarExpr "y") OpMult (LInt 3)), [])
      describe "invalid" $ do
        it "#1 (missing LHS)" $ evaluate (parseExpr [PLUS, DIGIT 3]) `shouldThrow` anyErrorCall
        it "#2 (invalid RHS)" $ evaluate (force (parseExpr [IDENT "x", PLUS, FUN, DIGIT 3])) `shouldThrow` anyErrorCall
        it "#3 (missing RHS)" $ evaluate (force (parseExpr [IDENT "x", PLUS])) `shouldThrow` anyErrorCall
  describe "Parser full file test cases:" $ do
    fullFileTestCases matchParseTestCase