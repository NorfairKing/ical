{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.URI.Gen where

import Data.GenValidity
import Data.GenValidity.URI ()
import ICal.PropertyType.URI

instance GenValid URI where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
