module ICal.TimeZoneSpec (spec) where

import Control.Arrow (left)
import qualified Data.ByteString as SB
import qualified Data.Text.Encoding as TE
import ICal.Component
import ICal.ContentLine
import ICal.UnfoldedLine
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  scenarioDir "test_resources/timezones" $ \tzFile -> do
    it "can parse this file as a timezone" $ do
      contents <- SB.readFile tzFile
      let errOrResult = do
            textContents <- left show $ TE.decodeUtf8' contents
            unfoldedLines <- parseUnfoldedLines textContents
            contentLines <- mapM parseContentLineFromUnfoldedLine unfoldedLines
            parseComponentFromContentLines contentLines

      case errOrResult of
        Left err -> expectationFailure err
        Right result -> shouldBeValid (result :: TimeZone)

    pending "roundtrip test as well"
