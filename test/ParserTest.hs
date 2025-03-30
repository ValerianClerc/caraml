{-# LANGUAGE LambdaCase #-}

module ParserTest where

import Common (Type (..))
import Control.DeepSeq (force)
import Control.Exception (SomeException, evaluate, try)
import Control.Exception.Base (fromException)
import Error (ParserException (..))
import qualified GHC.Exception.Type
import GHC.IO.Exception (assertError)
import Lexer
import Lib (Selection (Parser))
import Parser
import Test.Hspec
import TestCases
import TestUtils (fullFileTestCases, matchParseTestCase)

isParserException :: (ParserException -> Bool) -> SomeException -> Bool
isParserException predicate e = maybe False predicate (fromException e)

-- Matchers for specific error types
expectedToken :: SomeException -> Bool
expectedToken = isParserException $ \case
  ExpectedToken _ -> True
  _ -> False

endOfconditional :: SomeException -> Bool
endOfconditional = isParserException $ \case
  ExpectedEndOfConditional _ -> True
  _ -> False

extraTokens :: SomeException -> Bool
extraTokens = isParserException $ \case
  ExtraTokens _ -> True
  _ -> False

invalidFunctionArgs :: SomeException -> Bool
invalidFunctionArgs = isParserException $ \case
  InvalidFunctionDeclarationArgs _ -> True
  _ -> False

unexpectedToken :: SomeException -> Bool
unexpectedToken = isParserException $ \case
  UnexpectedToken _ -> True
  _ -> False

unexpectedEndOfExpression :: SomeException -> Bool
unexpectedEndOfExpression = isParserException $ \case
  UnexpectedEndOfExpression _ -> True
  _ -> False

parserTests :: [TestCase] -> Spec
parserTests testCases = do
  describe "Parser unit tests:" $ do
    describe "literals" $ do
      it "digit" $ parseExpr [DIGIT 2001] `shouldBe` (PInt 2001, [])
      it "char" $ parseExpr [CHAR 'c'] `shouldBe` (PChar 'c', [])
      it "string" $ parseExpr [STRING "testString"] `shouldBe` (PString "testString", [])
      it "boolean" $ parseExpr [BOOLEAN True] `shouldBe` (PBool True, [])
    describe "if-then-else" $ do
      describe "valid if" $ do
        it "#1" $ parseExpr [IF, BOOLEAN True, THEN, IDENT "x", ELSE, IDENT "y"] `shouldBe` (Conditional (PBool True) (VarExpr "x") (VarExpr "y"), [])
        it "#2" $ parseExpr [IF, IDENT "x", LST, DIGIT 0, THEN, IDENT "x", ELSE, IDENT "y"] `shouldBe` (Conditional (BinOp (VarExpr "x") OpLt (PInt 0)) (VarExpr "x") (VarExpr "y"), [])
      describe "invalid if" $ do
        it "#1 (missing THEN)" $ evaluate (parseExpr [IF, IDENT "x", LST, DIGIT 0, IDENT "x", ELSE, IDENT "y"]) `shouldThrow` endOfconditional
        it "#2 (invalid predicate)" $ evaluate (parseExpr [IF, EQU, THEN, IDENT "x", ELSE, IDENT "y"]) `shouldThrow` unexpectedToken
        it "#3 (invalid else)" $ evaluate (parseExpr [IF, BOOLEAN True, THEN, IDENT "x", ELSE, FUN]) `shouldThrow` unexpectedToken
    describe "variable declaration" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [LET, IDENT "x", EQU, DIGIT 3] `shouldBe` (Let "x" (PInt 3), [])
      describe "invalid" $ do
        it "#1 (missing variable identifier)" $ evaluate (parseExpr [LET, EQU, DIGIT 3]) `shouldThrow` unexpectedToken
        it "#2 (extra digit after identifier)" $ evaluate (parseExpr [LET, IDENT "x", DIGIT 3, EQU, BOOLEAN True]) `shouldThrow` unexpectedToken
    describe "function declaration" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COLON, KBOOL, COMMA, IDENT "p2", COLON, KINT, RPAREN, EQU, IDENT "p1"] `shouldBe` (FunDecl "x" [("p1", TBool), ("p2", TInt)] (VarExpr "p1"), [])
      describe "invalid" $ do
        it "#1 (missing right parens)" $ evaluate (force parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COLON, KBOOL, COMMA, IDENT "p2", COLON, KINT, EQU, IDENT "p1"]) `shouldThrow` unexpectedEndOfExpression
        it "#2 (extra digit after identifier)" $ evaluate (parseExpr [FUN, IDENT "x", DIGIT 3, LPAREN, IDENT "p1", COLON, KBOOL, COMMA, IDENT "p2", COLON, KINT, RPAREN, EQU, IDENT "p1"]) `shouldThrow` unexpectedToken
        it "#3 (missing comma)" $ evaluate (force (parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COLON, KBOOL, IDENT "p2", COLON, KINT, RPAREN, EQU, IDENT "p1"])) `shouldThrow` invalidFunctionArgs
        it "#4 (missing type annotations)" $ evaluate (force (parseExpr [FUN, IDENT "x", LPAREN, IDENT "p1", COMMA, IDENT "p2", RPAREN, EQU, IDENT "p1"])) `shouldThrow` invalidFunctionArgs
    describe "function application" $ do
      describe "valid" $ do
        it "#1" $ parseExpr [IDENT "x", LPAREN, DIGIT 3, COMMA, IDENT "p2", RPAREN] `shouldBe` (FunCall "x" [PInt 3, VarExpr "p2"], [])
        it "#2 (empty args)" $ parseExpr [IDENT "x", LPAREN, RPAREN] `shouldBe` (FunCall "x" [], [])
      describe "invalid" $ do
        it "#1 (missing right parens)" $ evaluate (parseExpr [IDENT "x", LPAREN, DIGIT 3, IDENT "p2"]) `shouldThrow` expectedToken
        it "#2 (no comma)" $ evaluate (parseExpr [IDENT "x", LPAREN, DIGIT 3, IDENT "p2"]) `shouldThrow` expectedToken
    describe "binary operation" $ do
      describe "valid" $ do
        it "#1 (plus)" $ parseExpr [IDENT "x", PLUS, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpPlus (PInt 3), [])
        it "#2 (minus)" $ parseExpr [IDENT "x", MINUS, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpMinus (PInt 3), [])
        it "#3 (mult)" $ parseExpr [IDENT "x", ASTERISK, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpMult (PInt 3), [])
        it "#4 (divide)" $ parseExpr [IDENT "x", DIVIDE, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpDiv (PInt 3), [])
        it "#5 (equal)" $ parseExpr [IDENT "x", EQU, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpEq (PInt 3), [])
        it "#6 (greater than)" $ parseExpr [IDENT "x", GRT, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpGt (PInt 3), [])
        it "#7 (greater than or equal)" $ parseExpr [IDENT "x", GEQ, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpGeq (PInt 3), [])
        it "#8 (less than)" $ parseExpr [IDENT "x", LST, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpLt (PInt 3), [])
        it "#9 (less than or equal)" $ parseExpr [IDENT "x", LEQ, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpLeq (PInt 3), [])
        it "#10 (not equal)" $ parseExpr [IDENT "x", NEQ, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpNeq (PInt 3), [])
        it "#11 (logical and)" $ parseExpr [IDENT "x", LAND, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpAnd (PInt 3), [])
        it "#12 (logical or)" $ parseExpr [IDENT "x", LOR, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpOr (PInt 3), [])
        it "#13 (nested)" $ parseExpr [IDENT "x", PLUS, IDENT "y", ASTERISK, DIGIT 3] `shouldBe` (BinOp (VarExpr "x") OpPlus (BinOp (VarExpr "y") OpMult (PInt 3)), [])
      describe "invalid" $ do
        it "#1 (missing LHS)" $ evaluate (parseExpr [PLUS, DIGIT 3]) `shouldThrow` unexpectedToken
        it "#2 (invalid RHS)" $ evaluate (force (parseExpr [IDENT "x", PLUS, FUN, DIGIT 3])) `shouldThrow` unexpectedToken
        it "#3 (missing RHS)" $ evaluate (force (parseExpr [IDENT "x", PLUS])) `shouldThrow` unexpectedEndOfExpression
  -- TODO: implement operator precedence + associativity
  -- describe "operator precedence" $ do
  -- describe "operator associativity" $ do
  describe "Parser full file test cases:" $ do
    fullFileTestCases matchParseTestCase