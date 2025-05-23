module TestUtils where

import Compile (compileAndRun)
import Control.Monad (void)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Lexer (runLex)
import Lib (Selection (..))
import Parser (runParser)
import Test.Hspec (SpecWith, it, shouldBe)
import TestCases
import ToLlvm (printLlvm, toLLVM)
import TypeInfer

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

matchTypeInferenceTestCase :: TestCase -> IO ()
matchTypeInferenceTestCase testCase =
  let parsed = parsedTestCase testCase
      typed = typedTestCase testCase
   in runTyper parsed `shouldBe` typed

matchLlvmTestCase :: TestCase -> IO ()
matchLlvmTestCase testCase = do
  let typed = typedTestCase testCase
      expected = expectedOutput testCase
  actual <- compileAndRun typed
  actual `shouldBe` Just expected

fullFileTestCases :: (TestCase -> IO ()) -> SpecWith ()
fullFileTestCases matchFun =
  foldl1 (>>) $
    map (\testCase -> it (rawTestCase testCase) $ matchFun testCase) testCases