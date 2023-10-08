import LexerTest
import Lib
import ParserTest
import Test.Hspec
import TestCases
import TypeTest (typeTests)

fileName = "./test/file.cml"

libTests = do
  describe "Lib tests" $ do
    describe "handleArgs" $ do
      it "passing in '--parser' and a valid file name" $ do
        handleArgs ["--parser", fileName] `shouldBe` (Parser, fileName)
      it "passing in '--lexer' and a valid file name" $ do
        handleArgs ["--lexer", fileName] `shouldBe` (Lexer, fileName)
      it "passing in an invalid string and a valid file name" $ do
        handleArgs ["aaaaaaaaaaaaa", fileName] `shouldBe` (Invalid, "")
      it "passing in an invalid number of arguments" $ do
        handleArgs ["--parser"] `shouldBe` (Invalid, "")

main :: IO ()
main = do
  hspec $ do
    libTests
    lexTests testCases
    parserTests testCases
    typeTests testCases
