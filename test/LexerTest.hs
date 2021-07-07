module LexerTest where

import           Control.Exception (evaluate)
import           Lexer
import           Test.Hspec
-- import           Test.QuickCheck

lexTests :: Spec
lexTests = do
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
      it "\"in\"" $ lexAlpha "in" `shouldBe` [IN, EOF]
      it "\"rec\"" $ lexAlpha "rec" `shouldBe` [REC, EOF]
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
      it "(" $ lexSymbol "(" `shouldBe` [LPAREN, EOF]
      it ")" $ lexSymbol ")" `shouldBe` [RPAREN, EOF]
      --it "\"" $ lexSymbol "\"" `shouldBe` [RPAREN, EOF]
      --it "'" $ lexSymbol "'" `shouldBe` [RPAREN, EOF]
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
      it "Invalid symbol error" $ evaluate (lexSymbol "~") `shouldThrow` anyErrorCall
