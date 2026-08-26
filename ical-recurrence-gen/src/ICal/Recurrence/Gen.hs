{-# OPTIONS_GHC -Wno-orphans #-}

module ICal.Recurrence.Gen where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import ICal.Component.Gen ()
import ICal.Property.Gen ()
import ICal.PropertyType.Gen ()
import ICal.PropertyType.RecurrenceRule.Gen ()
import ICal.Recurrence

instance GenValid Recurrence

instance GenValid Timestamp

instance GenValid RecurrenceEnd

instance (GenValid component) => GenValid (Recurring component)

instance (GenValid component) => GenValid (Occurrence component)

instance (GenValid component) => GenValid (Resolved component)
