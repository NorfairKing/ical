{-# LANGUAGE TypeApplications #-}

module ICal.Recurrence.TypesSpec where

import ICal
import ICal.Recurrence
import ICal.Recurrence.Gen ()
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Recurrence" $ do
    genValidSpec @Recurrence
  describe "RecurrenceEnd" $ do
    genValidSpec @RecurrenceEnd
  describe "Timestamp" $ do
    genValidSpec @Timestamp
  describe "Recurring" $ do
    genValidSpec @(Recurring Event)
    genValidSpec @(Recurring Todo)
    genValidSpec @(Recurring Journal)
  describe "Occurrence" $ do
    genValidSpec @(Occurrence Event)
    genValidSpec @(Occurrence Todo)
    genValidSpec @(Occurrence Journal)
  describe "Resolved" $ do
    genValidSpec @(Resolved Event)
