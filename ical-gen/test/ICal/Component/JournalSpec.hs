{-# LANGUAGE TypeApplications #-}

module ICal.Component.JournalSpec (spec) where

import ICal.Component
import ICal.Component.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Journal" $ do
    genValidSpec @Journal
    componentSpec @Journal

  componentScenarioDir @Journal "test_resources/journal"
