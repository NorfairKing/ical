{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertySpec where

import Data.Time
import ICal.Property
import ICal.Property.Gen
import ICal.PropertyType
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "ProdId" $ do
    genValidSpec @ProdId
    propertySpec @ProdId
  describe "prodIdP" $ do
    it "works for this example" $
      propertyContentLineP "PRODID:Example" `shouldBe` Right (ProdId "Example")

  describe "Version" $ do
    genValidSpec @Version
    propertySpec @Version
  describe "versionP" $ do
    it "works for this example" $
      propertyContentLineP "VERSION:2.0" `shouldBe` Right (Version "2.0")

  describe "UID" $ do
    genValidSpec @UID
    propertySpec @UID
  describe "uidP" $ do
    it "works for this example" $
      propertyContentLineP "UID:19960401T080045Z-4000F192713-0052@example.com" `shouldBe` Right (UID "19960401T080045Z-4000F192713-0052@example.com")

  describe "TZID" $ do
    genValidSpec @TZID
    propertySpec @TZID
  describe "tzIDP" $ do
    it "works for these examples" $ do
      propertyContentLineP "TZID:America/New_York" `shouldBe` Right (TZID "America/New_York")
      propertyContentLineP "TZID:America/Los_Angeles" `shouldBe` Right (TZID "America/Los_Angeles")
      propertyContentLineP "TZID:/example.org/America/New_York" `shouldBe` Right (TZID "/example.org/America/New_York")

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
  describe "dateTimeStampP" $ do
    it "works for these examplse" $ do
      propertyContentLineP "DTSTAMP:19971210T080000Z"
        `shouldBe` Right (DateTimeStamp (DateTimeUTC (LocalTime (fromGregorian 1997 12 10) (TimeOfDay 08 00 00))))
      propertyContentLineP "DTSTAMP:18581117T000000Z"
        `shouldBe` Right (DateTimeStamp (DateTimeUTC (LocalTime (fromGregorian 1858 11 17) (TimeOfDay 00 00 00))))

  describe "DateTimeStart" $ do
    genValidSpec @DateTimeStart
    propertySpec @DateTimeStart
  describe "dateTimeStartP" $ do
    it "works for this example" $
      propertyContentLineP "DTSTART:19980118T073000Z"
        `shouldBe` Right (DateTimeStartDateTime (DateTimeUTC (LocalTime (fromGregorian 1998 01 18) (TimeOfDay 07 30 00))))

  describe "Created" $ do
    genValidSpec @Created
    propertySpec @Created
  describe "createdP" $ do
    it "works for this example" $
      propertyContentLineP "CREATED:19960329T133000Z"
        `shouldBe` Right (Created (LocalTime (fromGregorian 1996 03 29) (TimeOfDay 13 30 00)))

  describe "Description" $ do
    genValidSpec @Description
    propertySpec @Description
    it "works for this example" $
      propertyContentLineP
        "DESCRIPTION:Meeting to provide technical review for \"Phoenix\"\n design.\\nHappy Face Conference Room. Phoenix design team\nMUST attend this meeting.\\nRSVP to team leader."
        `shouldBe` Right (Description "Meeting to provide technical review for \"Phoenix\" design.\nHappy Face Conference Room. Phoenix design team MUST attend this meeting.\nRSVP to team leader.")
