{-# LANGUAGE TypeApplications #-}

module ICal.Component.TimeZoneSpec (spec) where

import ICal.Component
import ICal.Component.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Observance" $ do
    genValidSpec @Observance

  describe "Standard" $ do
    genValidSpec @Standard
    componentSpec @Standard

  describe "Daylight" $ do
    genValidSpec @Daylight
    componentSpec @Daylight

  describe "TimeZone" $ do
    genValidSpec @TimeZoneObservance
    genValidSpec @Standard
    genValidSpec @Daylight
    genValidSpec @Observance
    genValidSpec @TimeZone
    componentSpec @TimeZone

  componentScenarioDir @TimeZone "test_resources/timezones"
