import           LexerTest
import           Lib
import           Test.Hspec
-- import           Test.QuickCheck

fileName = "./file.cml"

testCases :: [String]
testCases = [
  "let x = 3 in x + 4;",
  "fun If (x,y,z) = if x then y else z;",
  "fun fst (x,y) = x;",
  "fun fact (n) = if n=0 then 1 else n*fact(n-1);"
  ]

-- libTests :: IO ()
libTests = do
  describe "Lib tests" $ do
    describe "handleArgs" $ do
      it "passing in '--parser' and a valid file name" $ do
        handleArgs ["--parser", fileName] `shouldBe` (Parser, fileName)
      it "passing in '--lexer' and a valid file name" $ do
        handleArgs ["--lexer", fileName] `shouldBe` (Lexer, fileName)
      it "passing in an invalid string and a valid file name" $ do
        handleArgs ["aaaaaaaaaaaaa", fileName] `shouldBe` (Invalid, "")

main :: IO ()
main = do
  hspec $ do
    libTests
    lexTests testCases
