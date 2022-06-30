{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyTypeSpec where

import Control.Monad
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map as M
import Data.Text (Text)
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import ICal.ContentLine
import ICal.Parameter
import ICal.Parameter.Gen
import ICal.PropertyType
import ICal.PropertyType.Class
import ICal.PropertyType.Gen
import ICal.PropertyType.RecurrenceRule
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Text" $ do
    propertyTypeSpec @Text
    let examples :: [(Text, ContentLineValue)]
        examples =
          [ ( "Project XYZ Final Review\nConference Room - 3B\nCome Prepared.",
              mkSimpleContentLineValue "Project XYZ Final Review\\nConference Room - 3B\\nCome Prepared."
            )
          ]
    describe "examples" $
      forM_ examples $ \(text, clv) -> do
        it "renders this example text correctly" $
          propertyTypeB text `shouldBe` clv
        it "parses this example text correctly" $
          propertyTypeP clv `shouldBe` Right text
  describe "escapeText" $ do
    it "escapes this diverse example correctly" $
      escapeText "hello world\n\\,;," `shouldBe` "hello world\\n\\\\\\,\\;\\,"
    it "roundtrips with unEscapeText" $
      forAllValid $ \text -> escapeText (unEscapeText text) `shouldBe` text
  describe "unEscapeText" $ do
    it "unEscapes this diverse example correctly" $
      unEscapeText "hello world\\n\\N\\\\\\,\\;\\," `shouldBe` "hello world\n\n\\,;,"
    it "roundtrips with escapeText" $
      forAllValid $ \text -> unEscapeText (escapeText text) `shouldBe` text

  describe "DateTime" $ do
    genValidSpec @DateTime
    propertyTypeSpec @DateTime
  describe "dateTimeP" $ do
    -- Examples from the spec
    let examples :: [(DateTime, ContentLineValue)]
        examples =
          [ (DateTimeFloating $ LocalTime (fromGregorian 1998 01 18) (TimeOfDay 23 00 00), mkSimpleContentLineValue "19980118T230000"),
            (DateTimeUTC $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 07 00 00), mkSimpleContentLineValue "19980119T070000Z"),
            (DateTimeFloating $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 13 30 00), mkSimpleContentLineValue "19970714T133000"),
            (DateTimeUTC $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 17 30 00), mkSimpleContentLineValue "19970714T173000Z"),
            ( DateTimeZoned "America/New_York" $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 02 00 00),
              ContentLineValue
                { contentLineValueParams = M.singleton "TZID" ["America/New_York"],
                  contentLineValueRaw = "19980119T020000"
                }
            )
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
      case dateTimeP (mkSimpleContentLineValue "19980119T230000-0800") of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should have failed to parse."

  describe "Interval" $ do
    genValidSpec @Interval
    parameterSpec @Interval
    let examples :: [(NonEmpty ParamValue, Interval)]
        examples = [(["1"], Interval 1)]
    forM_ examples $ \(pvs, interval) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right interval
      it "can render this example" $
        parameterB interval `shouldBe` pvs

  describe "BySecond" $ do
    genValidSpec @BySecond
    parameterSpec @BySecond
    let examples :: [(NonEmpty ParamValue, BySecond)]
        examples = [(["1"], BySecond 1)]
    forM_ examples $ \(pvs, bySecond) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right bySecond
      it "can render this example" $
        parameterB bySecond `shouldBe` pvs

  describe "ByMinute" $ do
    genValidSpec @ByMinute
    parameterSpec @ByMinute
    let examples :: [(NonEmpty ParamValue, ByMinute)]
        examples = [(["1"], ByMinute 1)]
    forM_ examples $ \(pvs, byMinute) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byMinute
      it "can render this example" $
        parameterB byMinute `shouldBe` pvs

  describe "ByHour" $ do
    genValidSpec @ByHour
    parameterSpec @ByHour
    let examples :: [(NonEmpty ParamValue, ByHour)]
        examples = [(["1"], ByHour 1)]
    forM_ examples $ \(pvs, byHour) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byHour
      it "can render this example" $
        parameterB byHour `shouldBe` pvs

  describe "ByMonthDay" $ do
    genValidSpec @ByMonthDay
    parameterSpec @ByMonthDay
    let examples :: [(NonEmpty ParamValue, ByMonthDay)]
        examples = [(["1"], ByMonthDay 1)]
    forM_ examples $ \(pvs, byMonthDay) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byMonthDay
      it "can render this example" $
        parameterB byMonthDay `shouldBe` pvs

  describe "ByYearDay" $ do
    genValidSpec @ByYearDay
    parameterSpec @ByYearDay
    let examples :: [(NonEmpty ParamValue, ByYearDay)]
        examples = [(["1"], ByYearDay 1)]
    forM_ examples $ \(pvs, byYearDay) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byYearDay
      it "can render this example" $
        parameterB byYearDay `shouldBe` pvs

  describe "ByWeekNo" $ do
    genValidSpec @ByWeekNo
    parameterSpec @ByWeekNo
    let examples :: [(NonEmpty ParamValue, ByWeekNo)]
        examples = [(["1"], ByWeekNo 1)]
    forM_ examples $ \(pvs, byWeekNo) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byWeekNo
      it "can render this example" $
        parameterB byWeekNo `shouldBe` pvs

  describe "ByMonth" $ do
    genValidSpec @ByMonth
    parameterSpec @ByMonth
    let examples :: [(NonEmpty ParamValue, ByMonth)]
        examples = [(["1"], ByMonth January)]
    forM_ examples $ \(pvs, byMonth) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right byMonth
      it "can render this example" $
        parameterB byMonth `shouldBe` pvs

  describe "BySetPos" $ do
    genValidSpec @BySetPos
    parameterSpec @BySetPos
    let examples :: [(NonEmpty ParamValue, BySetPos)]
        examples = [(["1"], BySetPos 1)]
    forM_ examples $ \(pvs, bySetPos) -> do
      it "can parse this example" $
        parameterP pvs `shouldBe` Right bySetPos
      it "can render this example" $
        parameterB bySetPos `shouldBe` pvs

  describe "RecurrenceRule" $ do
    genValidSpec @RecurrenceRule
    propertyTypeSpec @RecurrenceRule
    let examples :: [(ContentLineValue, RecurrenceRule)]
        examples = []
    forM_ examples $ \(clv, recurrenceRule) -> do
      it "can parse this example" $
        propertyTypeP clv `shouldBe` Right recurrenceRule
      it "can render this example" $
        propertyTypeB recurrenceRule `shouldBe` clv
