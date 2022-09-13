{-# OPTIONS_GHC -Wno-orphans #-}

module ICal.Recurrence.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import ICal.Property.Gen ()
import ICal.PropertyType.Gen ()
import ICal.PropertyType.RecurrenceRule.Gen ()
import ICal.Recurrence

instance GenValid Recurrence

instance GenValid RecurringEvent

instance GenValid EventOccurrence
