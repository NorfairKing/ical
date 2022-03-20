{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Calendar.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import ICal.Calendar

instance GenValid Calendar

instance GenValid Event

instance GenValid ProdId

instance GenValid Version
