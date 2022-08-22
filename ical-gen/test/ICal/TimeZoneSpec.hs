{-# LANGUAGE TypeApplications #-}

module ICal.TimeZoneSpec (spec) where

import ICal.Component
import ICal.Component.Gen
import Test.Syd

spec :: Spec
spec = do
  componentScenarioDir @TimeZone "test_resources/timezones"
