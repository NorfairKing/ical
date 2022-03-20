{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.VCalendar.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import ICal.VCalendar

instance GenValid Calendar

instance GenValid Event

instance GenValid ProdId

instance GenValid Version
