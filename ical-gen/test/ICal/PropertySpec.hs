{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ICal.PropertySpec where

import ICal.Property
import ICal.Property.Gen
import Test.Syd
import Test.Syd.Validity

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

  describe "DateTimeStamp" $ do
    genValidSpec @DateTimeStamp
    propertySpec @DateTimeStamp
  describe "dateTimeStampP" $ do
    -- TODO example-based test
    pure ()

  describe "DateTimeStart" $ do
    genValidSpec @DateTimeStart
    propertySpec @DateTimeStart
  describe "dateTimeStartP" $ do
    -- TODO example-based test
    pure ()
