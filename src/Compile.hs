module Compile where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import GHC.IO.Exception (ExitCode (ExitSuccess))
import LLVM.Context (withContext)
import LLVM.Module (moduleLLVMAssembly, withModuleFromAST)
import System.Directory (removeFile)
import System.Process (readProcessWithExitCode)
import Text.Read (readMaybe)
import ToLlvm (toLLVM)
import TypeInfer (TypedExpr)

runtimeSrc :: String
runtimeSrc = "./runtime/runtime.c"

compileAndRun :: [TypedExpr] -> IO (Maybe Int)
compileAndRun exprs = do
  let llvmModule = toLLVM exprs
  llvmIR <- withContext $ \context ->
    withModuleFromAST context llvmModule $ \m ->
      moduleLLVMAssembly m

  let llFile = "temp_test.ll"
  let objFile = "temp_test.o"
  let exeFile = "temp_test_exe"
  BS.writeFile llFile llvmIR

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