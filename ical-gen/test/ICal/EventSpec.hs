{-# LANGUAGE TypeApplications #-}

module ICal.EventSpec (spec) where

import ICal.Component
import ICal.Component.Gen
import Test.Syd

spec :: Spec
spec = do
  componentScenarioDir @Event "test_resources/events"
