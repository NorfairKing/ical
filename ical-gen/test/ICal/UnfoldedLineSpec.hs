{-# LANGUAGE OverloadedStrings #-}

module ICal.UnfoldedLineSpec where

import ICal.UnfoldedLine
import ICal.UnfoldedLine.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseUnfoldedLinesByteString" $
    it "roundtrips with renderUnfoldedLinesByteString" $
      forAllValid $ \unfoldedLines ->
        parseUnfoldedLinesByteString (renderUnfoldedLinesByteString unfoldedLines)
          `shouldBe` Right unfoldedLines
  describe "parseUnfoldedLinesText" $ do
    it "parses empty text as no lines" $
      parseUnfoldedLinesText "" `shouldBe` Right []
    it "parses an empty line as such" $
      parseUnfoldedLinesText "\r\n" `shouldBe` Right [""]
    it "roundtrips with renderUnfoldedLinesText" $
      forAllValid $ \unfoldedLines ->
        parseUnfoldedLinesText (renderUnfoldedLinesText unfoldedLines)
          `shouldBe` Right unfoldedLines
  describe "renderUnfoldedLinesText" $ do
    it "renders no lines as nothing" $
      renderUnfoldedLinesText [] `shouldBe` ""
    it "renders an empty line as such" $
      renderUnfoldedLinesText [""] `shouldBe` "\r\n"
    it "produces the same folded line as before" $
      pureGoldenTextFile
        "test_resources/unfolded-lines.txt"
        ( renderUnfoldedLinesText
            [ "", -- empty line
              "This is rather a short line.",
              "This is a very long line which definitely contains more than seventy five characters.",
              "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
            ]
        )
