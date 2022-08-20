module ICal.TimeZoneSpec (spec) where

import Control.Arrow (left)
import qualified Data.ByteString as SB
import qualified Data.DList as DList
import qualified Data.Text.Encoding as TE
import ICal.Component
import ICal.ContentLine
import ICal.UnfoldedLine
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  scenarioDir "test_resources/timezones" $ \tzFile ->
    it "can parse this file as a timezone and roundtrip it" $ do
      let parseBS bs = do
            textContents <- left show $ TE.decodeUtf8' bs
            unfoldedLines <- parseUnfoldedLines textContents
            contentLines <- mapM parseContentLineFromUnfoldedLine unfoldedLines
            parseComponentFromContentLines contentLines

          renderBS =
            TE.encodeUtf8
              . renderContentLines
              . DList.toList
              . componentSectionB

      contents <- SB.readFile tzFile
      case parseBS contents of
        Left err -> expectationFailure err
        Right result -> do
          shouldBeValid (result :: TimeZone)
          let rendered = renderBS result
          case parseBS rendered of
            Left err -> expectationFailure err
            Right result' -> result' `shouldBe` result
