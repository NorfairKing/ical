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

  describe "makeAudioAlarm" $ do
    it "produces valid alarms" $
      producesValid makeAudioAlarm

  describe "makeDisplayAlarm" $ do
    it "produces valid alarms" $
      producesValid2 makeDisplayAlarm

  describe "makeEmailAlarm" $ do
    it "produces valid alarms" $
      producesValid2 makeEmailAlarm

  componentScenarioDir @Alarm "test_resources/alarm"
