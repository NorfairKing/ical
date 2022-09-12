{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertyType.DateTimeSpec where

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
    propertyTypeRenderExampleSpec
      ( ContentLineValue
          { contentLineValueRaw = "19980118T230000",
            contentLineValueParams = M.singleton "VALUE" ["DATE-TIME"]
          }
      )
      (DateTimeFloating $ LocalTime (fromGregorian 1998 01 18) (TimeOfDay 23 00 00))

    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "19980118T230000")
      (DateTimeFloating $ LocalTime (fromGregorian 1998 01 18) (TimeOfDay 23 00 00))

    propertyTypeRenderExampleSpec
      ( ContentLineValue
          { contentLineValueRaw = "19980119T070000Z",
            contentLineValueParams = M.singleton "VALUE" ["DATE-TIME"]
          }
      )
      (DateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 07 00 00))

    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "19980119T070000Z")
      (DateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 07 00 00))

    propertyTypeRenderExampleSpec
      ( ContentLineValue
          { contentLineValueRaw = "19970714T133000",
            contentLineValueParams = M.singleton "VALUE" ["DATE-TIME"]
          }
      )
      (DateTimeFloating $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 13 30 00))

    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "19970714T133000")
      (DateTimeFloating $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 13 30 00))

    propertyTypeRenderExampleSpec
      ( ContentLineValue
          { contentLineValueRaw = "19970714T173000Z",
            contentLineValueParams = M.singleton "VALUE" ["DATE-TIME"]
          }
      )
      (DateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 17 30 00))

    propertyTypeParseExampleSpec
      (mkSimpleContentLineValue "19970714T173000Z")
      (DateTimeUTC $ localTimeToUTC utc $ LocalTime (fromGregorian 1997 07 14) (TimeOfDay 17 30 00))

    propertyTypeRenderExampleSpec
      ( ContentLineValue
          { contentLineValueParams =
              M.fromList
                [ ("TZID", ["America/New_York"]),
                  ("VALUE", ["DATE-TIME"])
                ],
            contentLineValueRaw = "19980119T020000"
          }
      )
      (DateTimeZoned "America/New_York" $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 02 00 00))

    propertyTypeParseExampleSpec
      ( ContentLineValue
          { contentLineValueParams = M.singleton "TZID" ["America/New_York"],
            contentLineValueRaw = "19980119T020000"
          }
      )
      (DateTimeZoned "America/New_York" $ LocalTime (fromGregorian 1998 01 19) (TimeOfDay 02 00 00))

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

    it "fails to parse this datetime with the wrong value type" $
      let clv =
            ContentLineValue
              { contentLineValueRaw = "19970714T173000Z",
                contentLineValueParams = M.singleton "VALUE" ["DATE"]
              }
       in dateTimeP clv `shouldBe` Left "Invalid VALUE"
