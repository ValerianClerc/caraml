import           Lib
import           Test.Hspec
-- import           Test.QuickCheck

fileName = "./file.cml"

main :: IO ()
main = spec

spec :: IO ()
spec = hspec $ do
  describe "Lib tests" $ do
    describe "handleArgs" $ do
      it "passing in '--parser' and a valid file name" $ do
        handleArgs ["--parser", fileName] `shouldBe` (Parser, fileName)
      it "passing in '--lexer' and a valid file name" $ do
        handleArgs ["--lexer", fileName] `shouldBe` (Lexer, fileName)
      it "passing in an invalid string and a valid file name" $ do
        handleArgs ["aaaaaaaaaaaaa", fileName] `shouldBe` (Invalid, "")
