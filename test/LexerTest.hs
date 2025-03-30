{-# LANGUAGE BlockArguments #-}

module LexerTest where

import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Lexer
import Lib (Selection (Lexer))
import Test.Hspec
import TestCases
import TestUtils

lexTests :: [TestCase] -> Spec
lexTests testCases = do
  describe "Lexer tests" $ do
    describe "lexDigit" $ do
      it "\"2001\"" $ lexDigit "2001" `shouldBe` [DIGIT 2001, EOF]
      it "\"0\"" $ lexDigit "0" `shouldBe` [DIGIT 0, EOF]
      it "\"\"" $ lexDigit "" `shouldBe` [EOF]
      it "\"1 2\"" $ lexDigit "1 2" `shouldBe` [DIGIT 1, DIGIT 2, EOF]
      it "\"1=\"" $ lexDigit "1=" `shouldBe` [DIGIT 1, EQU, EOF]
    describe "lexAlpha" $ do
      it "\"not\"" $ lexAlpha "not" `shouldBe` [NOT, EOF]
      it "\"if\"" $ lexAlpha "if" `shouldBe` [IF, EOF]
      it "\"then\"" $ lexAlpha "then" `shouldBe` [THEN, EOF]
      it "\"else\"" $ lexAlpha "else" `shouldBe` [ELSE, EOF]
      it "\"let\"" $ lexAlpha "let" `shouldBe` [LET, EOF]
      it "\"letx\"" $ lexAlpha "letx" `shouldBe` [IDENT "letx", EOF]
      it "\"rec\"" $ lexAlpha "rec" `shouldBe` [REC, EOF]
      it "\"fun\"" $ lexAlpha "fun" `shouldBe` [FUN, EOF]
      it "\"int\"" $ lexAlpha "int" `shouldBe` [KINT, EOF]
      it "\"bool\"" $ lexAlpha "bool" `shouldBe` [KBOOL, EOF]
      it "\"true\"" $ lexAlpha "true" `shouldBe` [BOOLEAN True, EOF]
      it "\"false\"" $ lexAlpha "false" `shouldBe` [BOOLEAN False, EOF]
      it "\"x\"" $ lexAlpha "x" `shouldBe` [IDENT "x", EOF]
      it "\"not x\"" $ lexAlpha "not x" `shouldBe` [NOT, IDENT "x", EOF]
      it "\"if true then x else y\"" $ lexAlpha "if true then x else y" `shouldBe` [IF, BOOLEAN True, THEN, IDENT "x", ELSE, IDENT "y", EOF]
      it "\"\"" $ lexAlpha "" `shouldBe` [EOF]
      it "digit at the start of identifier (error)" $ evaluate (lexAlpha "1abc") `shouldThrow` anyErrorCall
      it "longer identifier" $ lexAlpha "Ahfjdksfh133lksjdf" `shouldBe` [IDENT "Ahfjdksfh133lksjdf", EOF]
    describe "lexParens" $ do
      it "single line comment" $ lexParens "* x true let y*) z" `shouldBe` [IDENT "z", EOF]
      it "not a comment (just normal parens)" $ lexParens "someVar) + 1" `shouldBe` [LPAREN, IDENT "someVar", RPAREN, PLUS, DIGIT 1, EOF]
      it "no closing comment" $ lexParens "* x true let y" `shouldBe` [EOF]
      it "multiline comment" $ lexParens "* x true \n let y\n *)" `shouldBe` [EOF]
    describe "lexSymbol" $ do
      it "empty str" $ lexSymbol "" `shouldBe` [EOF]
      it "(" $ lexSymbol "(" `shouldBe` [LPAREN, EOF]
      it ")" $ lexSymbol ")" `shouldBe` [RPAREN, EOF]
      it "double quotes (string)" $ lexSymbol "\"hello\"" `shouldBe` [STRING "hello", EOF]
      it "single quotes (char)" $ lexSymbol "'a'" `shouldBe` [CHAR 'a', EOF]
      it "-" $ lexSymbol "-" `shouldBe` [MINUS, EOF]
      it "+" $ lexSymbol "+" `shouldBe` [PLUS, EOF]
      it "*" $ lexSymbol "*" `shouldBe` [ASTERISK, EOF]
      it "/" $ lexSymbol "/" `shouldBe` [DIVIDE, EOF]
      it "=" $ lexSymbol "=" `shouldBe` [EQU, EOF]
      it ">" $ lexSymbol ">" `shouldBe` [GRT, EOF]
      it ">=" $ lexSymbol ">=" `shouldBe` [GEQ, EOF]
      it ">x" $ lexSymbol ">x" `shouldBe` [GRT, IDENT "x", EOF]
      it "<" $ lexSymbol "<" `shouldBe` [LST, EOF]
      it "<=" $ lexSymbol "<=" `shouldBe` [LEQ, EOF]
      it "<x" $ lexSymbol "<x" `shouldBe` [LST, IDENT "x", EOF]
      it "<>" $ lexSymbol "<>" `shouldBe` [NEQ, EOF]
      it "," $ lexSymbol "," `shouldBe` [COMMA, EOF]
      it "_" $ lexSymbol "_" `shouldBe` [UNDERSCORE, EOF]
      it ";" $ lexSymbol ";" `shouldBe` [SC, EOF]
      it ":" $ lexSymbol ":" `shouldBe` [COLON, EOF]
      it "&" $ lexSymbol "&" `shouldBe` [LAND, EOF]
      it "|" $ lexSymbol "|" `shouldBe` [LOR, EOF]
      it "~" $ lexSymbol "~" `shouldBe` [TILDE, EOF]
      it "Invalid symbol error" $ evaluate (lexSymbol "@") `shouldThrow` anyErrorCall
    describe "lexString" $ do
      it "normal string" $ lexString "\"hello\"" `shouldBe` [STRING "hello", EOF]
      it "empty string" $ lexString "\"\"" `shouldBe` [STRING "", EOF]
      it "only 1 double quote" $ evaluate (lexString "\"") `shouldThrow` anyErrorCall
      it "no double quotes" $ evaluate (lexString "x") `shouldThrow` anyErrorCall
    describe "lexChar" $ do
      it "normal char" $ lexChar "'c'" `shouldBe` [CHAR 'c', EOF]
      it "empty char" $ evaluate (lexChar "''") `shouldThrow` anyErrorCall
      it "not closed char" $ evaluate (lexChar "'") `shouldThrow` anyErrorCall
      it "escape char '\\n'" $ lexChar "'\\n'" `shouldBe` [CHAR '\n', EOF]
      it "escape char '\\t'" $ lexChar "'\\t'" `shouldBe` [CHAR '\t', EOF]
      it "escape char '\\r'" $ lexChar "'\\r'" `shouldBe` [CHAR '\r', EOF]
      it "escape char '\\b'" $ lexChar "'\\b'" `shouldBe` [CHAR '\b', EOF]
      it "invalid escape char" $ evaluate (force (lexChar "'\\x'" ++ [EOF])) `shouldThrow` anyErrorCall
    describe "runLex" $ do
      it "whitespace" $ runLex "     " `shouldBe` [EOF]
      it "number" $ runLex "3" `shouldBe` [DIGIT 3, EOF]
      it "keyword/identifier" $ runLex "let" `shouldBe` [LET, EOF]
      it "symbol" $ runLex "*" `shouldBe` [ASTERISK, EOF]
    describe "Lexer full file test cases" $ do
      fullFileTestCases matchLexTestCase