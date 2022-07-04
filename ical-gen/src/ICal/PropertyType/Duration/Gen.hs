{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.Duration.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import ICal.PropertyType.Duration

instance GenValid Duration

instance GenValid DurDate

instance GenValid DurTime

instance GenValid DurWeek

instance GenValid Sign
