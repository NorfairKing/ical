{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DateTimeSpec where

import Control.Monad
import qualified Data.Map as M
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian, localTimeToUTC, utc)
import ICal.ContentLine
import ICal.PropertyType.DateTime
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
            (DateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 07 00 00), mkSimpleContentLineValue "19980119T070000Z"),
            (DateTimeFloating $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 13 30 00), mkSimpleContentLineValue "19970714T133000"),
            (DateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 17 30 00), mkSimpleContentLineValue "19970714T173000Z"),
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
