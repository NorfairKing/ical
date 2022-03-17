{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.UnfoldedLine.Gen where

import Data.GenValidity
import Data.GenValidity.Text ()
import ICal.UnfoldedLine

instance GenValid UnfoldedLine
