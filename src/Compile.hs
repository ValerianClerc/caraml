module Compile where

import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Directory (removeFile)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import ToLlvm (printLlvm, toLLVM)
import TypeInfer (TypedExpr)

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