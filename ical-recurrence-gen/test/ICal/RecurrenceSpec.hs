{-# LANGUAGE TypeApplications #-}

module ICal.RecurrenceSpec where

import ICal.Recurrence
import ICal.Recurrence.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Recurrence" $ do
    genValidSpec @Recurrence
  describe "RecurringEvent" $ do
    genValidSpec @RecurringEvent
  describe "EventOccurrence" $ do
    genValidSpec @EventOccurrence
