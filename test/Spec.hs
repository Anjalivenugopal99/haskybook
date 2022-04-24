import Test.Hspec
import Text.Parsec (ParseError)
import Lib (parseText, parseParagraph, parseBody, Block(..), regularParse)
import Control.Exception (evaluate)

getBlock :: Either ParseError a -> a
getBlock (Right blk) = blk
getBlock (Left err) = error $ show err

main :: IO ()
main = hspec $ do
  describe "parseParagraph" $ do
    it "should work with bold over several lines" $ do
      getBlock (regularParse parseParagraph "Bob\nBob\nBob") `shouldBe` Paragraph [Text "Bob Bob Bob"]
  describe "parseBody" $ do
    it "should work with headings" $ do
    --   getBlock (regularParse parseBody "# Bob") `shouldBe` [Heading 1 [Text "Bob"]]
      getBlock (regularParse parseBody "# Bob\n") `shouldBe` [Heading 1 [Text "Bob"]]