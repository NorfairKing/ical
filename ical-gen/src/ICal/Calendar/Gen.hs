{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Calendar.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import ICal.Calendar
import ICal.Parameter.Gen ()
import ICal.PropertyType.Gen ()

instance GenValid Calendar

instance GenValid ProdId

instance GenValid Version

instance GenValid UID

instance GenValid DateTimeStamp

instance GenValid TZID

instance GenValid Event

instance GenValid TimeZone
