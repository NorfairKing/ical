{-# LANGUAGE TypeApplications #-}

module ICal.Component.EventSpec (spec) where

import ICal.Component
import ICal.Component.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Event" $ do
    genValidSpec @Event
    componentSpec @Event

  componentScenarioDir @Event "test_resources/events"
