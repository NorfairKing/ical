{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ICal.CalendarSpec where

import Control.Monad
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (TimeOfDay (..), fromGregorian)
import ICal.Calendar
import ICal.Calendar.Gen ()
import ICal.ContentLine
import ICal.UnfoldedLine
import Test.QuickCheck
import Test.Syd
import Test.Syd.Validity
import Text.Megaparsec

spec :: Spec
spec = do
  describe "ProdId" $ do
    genValidSpec @ProdId
  describe "prodIdP" $ do
    parseSucceedsSpec prodIdP ["PRODID:Example"] (ProdId "Example")
    it "roundtrips with prodIdB" $ parserBuilderRoundtrip prodIdP prodIdB

  describe "Version" $
    genValidSpec @Version
  describe "versionP" $ do
    parseSucceedsSpec versionP ["VERSION:2.0"] (Version "2.0")
    it "roundtrips with versionB" $ parserBuilderRoundtrip versionP versionB

  describe "UID" $
    genValidSpec @UID
  describe "uidP" $ do
    parseSucceedsSpec uidP ["UID:19960401T080045Z-4000F192713-0052@example.com"] (UID "19960401T080045Z-4000F192713-0052@example.com")
    it "roundtrips with uidB" $ parserBuilderRoundtrip uidP uidB

  describe "DateTime" $
    genValidSpec @DateTime
  describe "parseDateTime" $ do
    -- TODO example-based test
    it "roundtrips with renderDateTime" $
      forAllValid $ \dateTime ->
        case parseDateTime (renderDateTime dateTime) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` dateTime

  describe "Date" $
    genValidSpec @Date
  describe "parseDate" $ do
    let examples :: [(Date, Text)]
        examples =
          [ (Date $ fromGregorian 1997 07 14, "19970714")
          ]
    describe "examples" $
      forM_ examples $ \(date, text) -> do
        it "renders this example date correctly" $
          renderDate date `shouldBe` text
        it "parses this example text correctly" $
          parseDate text `shouldBe` Right date

    it "roundtrips with renderDate" $
      forAllValid $ \date ->
        case parseDate (renderDate date) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` date

  describe "Time" $
    genValidSpec @Time
  describe "parseTime" $ do
    let examples :: [(Time, Text)]
        examples =
          [ (TimeFloating $ TimeOfDay 23 00 00, "230000"),
            (TimeUTC $ TimeOfDay 07 00 00, "070000Z")
          ]
    describe "examples" $
      forM_ examples $ \(time, text) -> do
        it "renders this example time correctly" $
          renderTime time `shouldBe` text
        it "parses this example text correctly" $
          parseTime text `shouldBe` Right time

    it "fails to parse this counterexample from the spec" $
      case parseTime "230000-0800" of
        Left _ -> pure ()
        Right time -> expectationFailure $ "Should have failed to parse, but parsed: " <> show time

    it "roundtrips with renderTime" $
      forAllValid $ \time ->
        case parseTime (renderTime time) of
          Left err -> expectationFailure err
          Right actual -> actual `shouldBe` time

  describe "DateTimeStamp" $
    genValidSpec @DateTimeStamp
  describe "dateTimeStampP" $ do
    -- TODO example-based test
    it "roundtrips with dateTimeStampB" $ parserBuilderRoundtrip dateTimeStampP dateTimeStampB

  describe "Event" $
    genValidSpec @Event
  describe "vEventP" $
    it "roundtrips with vEventB" $ parserBuilderRoundtrip vEventP vEventB

  describe "TimeZone" $
    genValidSpec @TimeZone
  describe "vTimeZoneP" $
    it "roundtrips with vTimeZoneB" $ parserBuilderRoundtrip vTimeZoneP vTimeZoneB

  describe "Calendar" $
    genValidSpec @Calendar
  describe "vCalendarP" $
    it "roundtrips with vCalendarB" $ parserBuilderRoundtrip vCalendarP vCalendarB

parseSucceedsSpec ::
  (Show a, Eq a) =>
  CP a ->
  [ContentLine] ->
  a ->
  Spec
parseSucceedsSpec parser contentLines expected =
  it "parses these content lines correctly" $
    case parse parser "test input" contentLines of
      Left err -> expectationFailure $ errorBundlePretty err
      Right actual -> actual `shouldBe` expected

parserBuilderRoundtrip ::
  (Show a, Eq a, GenValid a) =>
  CP a ->
  (a -> DList ContentLine) ->
  Property
parserBuilderRoundtrip parser builder = forAllValid $ \a ->
  let rendered = DList.toList $ builder a
      renderedText = renderUnfoldedLinesText $ map renderContentLine rendered
      ctx =
        unlines
          [ "Internal representation:",
            ppShow rendered,
            "",
            "Textual representation:",
            T.unpack renderedText
          ]
   in context ctx $
        case parse parser "test input" rendered of
          Left err -> expectationFailure $ errorBundlePretty err
          Right parsed -> parsed `shouldBe` a
