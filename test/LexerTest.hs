module LexerTest where

import           Lexer
import           Test.Hspec
-- import           Test.QuickCheck

-- lexTests :: IO ()
lexTests = do
  describe "Lexer tests" $ do
    describe "lexDigit" $ do
      it "lexing the string \"2001\" should produce [DIGIT 2001, EOF]" $ do
        lexDigit "2001" `shouldBe` [DIGIT 2001, EOF]
      it "lexing the string \"0\" should produce [DIGIT 0, EOF]" $ do
        lexDigit "0" `shouldBe` [DIGIT 0, EOF]
      it "lexing the string \"\" should produce [EOF]" $ do
        lexDigit "" `shouldBe` [EOF]
      it "lexing the string \"1 2\" should produce [DIGIT 1, DIGIT 2, EOF]" $ do
        lexDigit "1 2" `shouldBe` [DIGIT 1, DIGIT 2, EOF]
      it "lexing the string \"1=\" should produce [DIGIT 1, EQ, EOF]" $ do
        lexDigit "1=" `shouldBe` [DIGIT 1, EQU, EOF]
