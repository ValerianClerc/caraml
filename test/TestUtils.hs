module TestUtils where

import Lexer (runLex)
import Lib (Selection (..))
import Parser (runParser)
import Test.Hspec (SpecWith, it, shouldBe)
import TestCases

matchLexTestCase :: TestCase -> IO ()
matchLexTestCase testCase =
  let raw = rawTestCase testCase
      lexed = lexedTestCase testCase
   in runLex raw `shouldBe` lexed

matchParseTestCase :: TestCase -> IO ()
matchParseTestCase testCase =
  let lexed = lexedTestCase testCase
      parsed = parsedTestCase testCase
   in runParser lexed `shouldBe` parsed

fullFileTestCases :: (TestCase -> IO ()) -> SpecWith ()
fullFileTestCases matchFun =
  foldl1 (>>) $
    map (\testCase -> it (rawTestCase testCase) $ matchFun testCase) testCases