module ParserTest where

import Control.Exception (evaluate)
import Lexer
import Lib (Selection (Parser))
import Parser
import Test.Hspec
import TestCases
import TestUtils (fullFileTestCases, matchParseTestCase)

parserTests :: [TestCase] -> Spec
parserTests testCases = do
  describe "Parser tests" $ do
    it "" $ do pending

-- describe "parseExpr unit tests" $ do
--   it "" $ parseExpr [DIGIT 2001, EOF] `shouldBe` (LInt 2001, [])
-- describe "parseExpr full file tests" $ do
--   fullFileTestCases matchParseTestCase
