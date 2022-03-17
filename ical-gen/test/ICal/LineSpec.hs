module ICal.LineSpec where

import ICal.Line
import ICal.Line.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "parseUnfoldedLinesByteString" $
    it "roundtrips with renderUnfoldedLinesByteString" $
      forAllValid $ \unfoldedLines ->
        parseUnfoldedLinesByteString (renderUnfoldedLinesByteString unfoldedLines)
          `shouldBe` Right unfoldedLines
  describe "parseUnfoldedLinesText" $
    it "roundtrips with renderUnfoldedLinesText" $
      forAllValid $ \unfoldedLines ->
        parseUnfoldedLinesText (renderUnfoldedLinesText unfoldedLines)
          `shouldBe` Right unfoldedLines
