{-# LANGUAGE TypeApplications #-}

module ICal.Component.AlarmSpec (spec) where

import ICal.Component
import ICal.Component.Alarm
import ICal.Component.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Alarm" $ do
    genValidSpec @Alarm
    componentSpec @Alarm

  describe "makeAlarm" $ do
    it "should produce valid alarms" $
      producesValid2 makeAlarm

  componentScenarioDir @Alarm "test_resources/alarm"
