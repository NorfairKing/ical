{-# LANGUAGE TypeApplications #-}

module ICal.Component.TodoSpec (spec) where

import ICal.Component
import ICal.Component.Gen
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Todo" $ do
    genValidSpec @Todo
    componentSpec @Todo

  componentScenarioDir @Todo "test_resources/todo"
