{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.Line.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import ICal.Line

instance GenValid UnfoldedLine
