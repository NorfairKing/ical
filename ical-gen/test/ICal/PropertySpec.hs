{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertySpec where

import Data.Time
import ICal.Property
import ICal.Property.Gen
import ICal.PropertyType.Date
import ICal.PropertyType.DateTime
import ICal.PropertyType.Duration
import ICal.PropertyType.Duration.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "ProdId" $ do
    genValidSpec @ProdId
    propertySpec @ProdId
    it "works for this example" $
      propertyContentLineP "PRODID:Example" `shouldBe` Right (ProdId "Example")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
    it "works for this example" $
      propertyContentLineP "VERSION:2.0" `shouldBe` Right (Version "2.0")

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID
    it "works for this example" $
      propertyContentLineP "UID:19960401T080045Z-4000F192713-0052@example.com" `shouldBe` Right (UID "19960401T080045Z-4000F192713-0052@example.com")

  describe "TZID" $ do
    genValidSpec @TZID
    propertySpec @TZID
    it "works for these examples" $ do
      propertyContentLineP "TZID:America/New_York" `shouldBe` Right (TZID "America/New_York")
      propertyContentLineP "TZID:America/Los_Angeles" `shouldBe` Right (TZID "America/Los_Angeles")
      propertyContentLineP "TZID:/example.org/America/New_York" `shouldBe` Right (TZID "/example.org/America/New_York")

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
    it "works for these examplse" $ do
      propertyContentLineP "DTSTAMP:19971210T080000Z"
        `shouldBe` Right (DateTimeStamp (DateTimeUTC (LocalTime (fromGregorian 1997 12 10) (TimeOfDay 08 00 00))))
      propertyContentLineP "DTSTAMP:18581117T000000Z"
        `shouldBe` Right (DateTimeStamp (DateTimeUTC (LocalTime (fromGregorian 1858 11 17) (TimeOfDay 00 00 00))))

  describe "DateTimeStart" $ do
    genValidSpec @DateTimeStart
    propertySpec @DateTimeStart
    it "works for this example" $
      propertyContentLineP "DTSTART:19980118T073000Z"
        `shouldBe` Right (DateTimeStartDateTime (DateTimeUTC (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00))))

  describe "Classification" $ do
    genValidSpec @Classification
    propertySpec @Classification
    it "works for this example" $
      propertyContentLineP "CLASS:PUBLIC"
        `shouldBe` Right ClassificationPublic

  describe "Created" $ do
    genValidSpec @Created
    propertySpec @Created
    it "works for this example" $
      propertyContentLineP "CREATED:19960329T133000Z"
        `shouldBe` Right (Created (LocalTime (fromGregorian 1996 03 29) (TimeOfDay 13 30 00)))

  describe "Summary" $ do
    genValidSpec @Summary
    propertySpec @Summary
    it "works for this example" $
      propertyContentLineP
        "SUMMARY:Department Party"
        `shouldBe` Right (Summary "Department Party")

  describe "Description" $ do
    genValidSpec @Description
    propertySpec @Description
    it "works for this example" $
      propertyContentLineP
        "DESCRIPTION:Meeting to provide technical review for \"Phoenix\" design.\\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\\nRSVP to team leader."
        `shouldBe` Right (Description "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.")

  describe "GeographicPosition" $ do
    genValidSpec @GeographicPosition
    propertySpec @GeographicPosition
    it "works for this example" $
      propertyContentLineP "GEO:37.386013;-122.082932"
        `shouldBe` Right
          ( GeographicPosition
              { geographicPositionLat = 37.386013,
                geographicPositionLon = -122.082932
              }
          )

  describe "DateTimeEnd" $ do
    genValidSpec @DateTimeEnd
    propertySpec @DateTimeEnd
    -- From the spec:
    -- @
    --     Example:  The following is an example of this property:
    --
    --         DTEND:19960401T150000Z
    --
    --         DTEND;VALUE=DATE:19980704
    -- @
    it "works for this example" $
      propertyContentLineP
        "DTEND:19960401T150000Z"
        `shouldBe` Right (DateTimeEndDateTime (DateTimeUTC (LocalTime (fromGregorian 1996 04 01) (TimeOfDay 15 00 00))))
    it "works for this example" $
      propertyContentLineP
        "DTEND;VALUE=DATE:19980704"
        `shouldBe` Right (DateTimeEndDate (Date (fromGregorian 1998 07 04)))

  describe "Duration" $ do
    genValidSpec @Duration
    propertySpec @Duration
    -- From the spec:
    -- @
    --     Example:  The following is an example of this property that specifies
    --        an interval of time of one hour and zero minutes and zero seconds:
    --
    --         DURATION:PT1H0M0S
    --
    --        The following is an example of this property that specifies an
    --        interval of time of 15 minutes.
    --
    --         DURATION:PT15M
    -- @
    it "works for this example" $
      propertyContentLineP
        "DURATION:PT1H0M0S"
        `shouldBe` Right
          ( DurationTime
              ( DurTime
                  { durTimeSign = Positive,
                    durTimeHour = 1,
                    durTimeMinute = 0,
                    durTimeSecond = 0
                  }
              )
          )
    it "works for this example" $
      propertyContentLineP
        "DURATION:PT15M"
        `shouldBe` Right
          ( DurationTime
              ( DurTime
                  { durTimeSign = Positive,
                    durTimeHour = 0,
                    durTimeMinute = 15,
                    durTimeSecond = 0
                  }
              )
          )

  describe "TimeZoneName" $ do
    xdescribe "already in DurationSpec" $ genValidSpec @TimeZoneName
    propertySpec @TimeZoneName
    pending "works for this example"
