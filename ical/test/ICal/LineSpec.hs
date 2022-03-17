{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.LineSpec where

import Data.GenValidity
import Data.GenValidity.Text ()
import ICal.Line
import Test.Syd
import Test.Syd.Validity

instance GenValid UnfoldedLine

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
