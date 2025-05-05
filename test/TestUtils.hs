module TestUtils where

import Control.Monad (void)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import Lexer (runLex)
import Lib (Selection (..))
import Parser (runParser)
import System.Directory (removeFile)
import System.Process (readProcessWithExitCode)
import Test.Hspec (SpecWith, it, shouldBe)
import TestCases
import Text.Read (readMaybe)
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

compileAndRun :: [TypedExpr] -> IO (Maybe Int)
compileAndRun exprs = do
  let llvmModule = toLLVM exprs
  let llvmIR = printLlvm llvmModule

  let llFile = "temp_test.ll"
  let objFile = "temp_test.o"
  let exeFile = "temp_test_exe"
  let runtimeSrc = "runtime/runtime.c"
  writeFile llFile llvmIR

  (exitCodeClang, _, stderrClang) <-
    readProcessWithExitCode "clang" ["-Wno-override-module", "-lm", llFile, runtimeSrc, "-o", exeFile] ""

  removeFile llFile
  if exitCodeClang /= ExitSuccess
    then do
      putStrLn $ "clang compilation/linking failed: " ++ stderrClang
      _ <- readProcessWithExitCode "rm" ["-f", exeFile] ""
      return Nothing
    else do
      (exitCodeRun, stdoutRun, stderrRun) <- readProcessWithExitCode ("./" ++ exeFile) [] ""
      removeFile exeFile
      if exitCodeRun /= ExitSuccess
        then do
          putStrLn $ "Execution failed: " ++ stderrRun
          return Nothing
        else do
          return $ readMaybe (filter (/= '\n') stdoutRun)

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