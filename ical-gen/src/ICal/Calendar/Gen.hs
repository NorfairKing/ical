{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Calendar.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import ICal.Calendar
import ICal.Property.Gen ()

instance GenValid Calendar

instance GenValid Event

instance GenValid TimeZone
