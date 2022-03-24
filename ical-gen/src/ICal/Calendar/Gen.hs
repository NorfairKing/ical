{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Calendar.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import ICal.Calendar

instance GenValid Calendar

instance GenValid ProdId

instance GenValid Version

instance GenValid UID

instance GenValid DateTimeStamp

instance GenValid Date

instance GenValid Time

instance GenValid DateTime

instance GenValid Event

instance GenValid TimeZone
