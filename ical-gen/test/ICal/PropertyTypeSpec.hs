{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyTypeSpec where

import Control.Monad
import qualified Data.Map as M
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)
import ICal.ContentLine
import ICal.PropertyType
import ICal.PropertyType.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
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

  describe "Date" $ do
    genValidSpec @Date
    propertyTypeSpec @Date
  describe "dateP" $ do
    let examples :: [(Date, ContentLineValue)]
        examples =
          [ (Date $ fromGregorian 1997 07 14, mkSimpleContentLineValue "19970714")
          ]
    describe "examples" $
      forM_ examples $ \(date, text) -> do
        it "renders this example date correctly" $
          dateB date `shouldBe` text
        it "parses this example text correctly" $
          dateP text `shouldBe` Right date

  describe "Time" $ do
    genValidSpec @Time
    propertyTypeSpec @Time
  describe "timeP" $ do
    let examples :: [(Time, ContentLineValue)]
        examples =
          [ (TimeFloating $ TimeOfDay 23 00 00, mkSimpleContentLineValue "230000"),
            (TimeUTC $ TimeOfDay 07 00 00, mkSimpleContentLineValue "070000Z"),
            ( TimeZoned "America/New_York" (TimeOfDay 08 30 00),
              ContentLineValue
                { contentLineValueParams = M.singleton "TZID" ["America/New_York"],
                  contentLineValueRaw = "083000"
                }
            )
          ]
    describe "examples" $
      forM_ examples $ \(time, text) -> do
        it "renders this example time correctly" $
          timeB time `shouldBe` text
        it "parses this example text correctly" $
          timeP text `shouldBe` Right time

    it "fails to parse this counterexample from the spec" $
      case timeP (mkSimpleContentLineValue "230000-0800") of
        Left _ -> pure ()
        Right time -> expectationFailure $ "Should have failed to parse, but parsed: " <> show time