{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.CalendarSpec where

import Control.Monad
import Data.DList (DList (..))
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
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
    propertySpec @ProdId
  describe "prodIdP" $ do
    it "works for this example" $
      prodIdP "PRODID:Example" `shouldBe` Right (ProdId "Example")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
  describe "versionP" $ do
    it "works for this example" $
      versionP "VERSION:2.0" `shouldBe` Right (Version "2.0")

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID
  describe "uidP" $ do
    it "works for this example" $
      uidP "UID:19960401T080045Z-4000F192713-0052@example.com" `shouldBe` Right (UID "19960401T080045Z-4000F192713-0052@example.com")

  describe "TZID" $ do
    genValidSpec @TZID
    propertySpec @TZID
  describe "tzIDP" $ do
    it "works for these examples" $ do
      tzIDP "TZID:America/New_York" `shouldBe` Right (TZID "America/New_York")
      tzIDP "TZID:America/Los_Angeles" `shouldBe` Right (TZID "America/Los_Angeles")
      tzIDP "TZID:/example.org/America/New_York" `shouldBe` Right (TZID "/example.org/America/New_York")

  describe "TZIDParam" $ do
    genValidSpec @TZIDParam
    parameterSpec @TZIDParam
  describe "tzIDP" $ do
    it "works for these examples" $ do
      tzIDParamP ["America/New_York"] `shouldBe` Right (TZIDParam "America/New_York")
      tzIDParamP ["/example.org/America/New_York"] `shouldBe` Right (TZIDParam "/example.org/America/New_York")

  describe "DateTime" $ do
    genValidSpec @DateTime
    propertyTypeSpec @DateTime
  describe "dateTimeP" $ do
    -- Examples from the spec
    let examples :: [(DateTime, ContentLineValue)]
        examples =
          [ (DateTimeFloating $ LocalTime (fromGregorian 1998 01 18) (TimeOfDay 23 00 00), "19980118T230000"),
            (DateTimeUTC $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 07 00 00), "19980119T070000Z"),
            (DateTimeFloating $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 13 30 00), "19970714T133000"),
            (DateTimeUTC $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 17 30 00), "19970714T173000Z"),
            (DateTimeZoned "America/New_York" $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 02 00 00), "TZID=America/New_York:19980119T020000")
          ]
    describe "examples" $
      forM_ examples $ \(dateTime, text) -> do
        it "renders this example dateTime correctly" $
          dateTimeB dateTime `shouldBe` text
        it "parses this example text correctly" $
          dateTimeP text `shouldBe` Right dateTime
    -- @
    --       The form of date and time with UTC offset MUST NOT be used.  For
    --       example, the following is not valid for a DATE-TIME value:
    --
    --        19980119T230000-0800       ;Invalid time format
    -- @
    it "fails to parse this invalid datetime" $
      case dateTimeP "19980119T230000-0800" of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should have failed to parse."

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

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
  describe "dateTimeStampP" $ do
    -- TODO example-based test
    pure ()

  describe "Event" $ do
    genValidSpec @Event
    componentSpec @TimeZone

  describe "TimeZone" $ do
    genValidSpec @TimeZone
    componentSpec @TimeZone

  describe "Calendar" $ do
    genValidSpec @Calendar
    componentSpec @Calendar

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

componentSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsComponent a) =>
  Spec
componentSpec = do
  it "renders to a valid list of ContentLines" $
    forAllValid $ \component ->
      shouldBeValid $ DList.toList $ componentB (component :: a)
  it "roundtrips through ContentLines" $
    parserBuilderRoundtrip (componentP :: CP a) componentB

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

propertySpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsProperty a) =>
  Spec
propertySpec = do
  it "always renders to a valid content line" $
    forAllValid $ \property ->
      shouldBeValid $ propertyB (property :: a)
  it "roundtrips through ContentLine" $
    forAllValid $ \property ->
      let rendered = propertyB (property :: a)
       in case propertyP rendered of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` property

propertyTypeSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsPropertyType a) =>
  Spec
propertyTypeSpec = do
  it "always renders to a valid content line" $
    forAllValid $ \propertyType ->
      shouldBeValid $ propertyTypeB (propertyType :: a)
  it "roundtrips through ContentLine" $
    forAllValid $ \propertyType ->
      let value = propertyTypeB (propertyType :: a)
       in case propertyTypeP value of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` propertyType

parameterSpec ::
  forall a.
  (Show a, Eq a, GenValid a, IsParameter a) =>
  Spec
parameterSpec = do
  it "always renders a valid parameter values" $
    forAllValid $ \parameter ->
      shouldBeValid $ parameterB (parameter :: a)
  it "roundtrips through parameter values" $
    forAllValid $ \parameter ->
      let values = parameterB (parameter :: a)
       in case parameterP values of
            Left err -> expectationFailure err
            Right actual -> actual `shouldBe` parameter
