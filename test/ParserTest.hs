module ParserTest where

import           Control.Exception (evaluate)
import           Lexer
import           Parser
import           Test.Hspec
import           TestCases


parserTests :: [TestCase] -> Spec
parserTests testCases = do
  describe "Parser tests" $ do
    describe "parseExpr unit tests" $ do
      it "\"2001\"" $ lexDigit "2001" `shouldBe` [DIGIT 2001, EOF] -- placeholder test: TODO remove
