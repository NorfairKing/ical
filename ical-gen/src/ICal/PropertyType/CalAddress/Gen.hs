{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.CalAddress.Gen where

import Data.GenValidity
import Data.GenValidity.ByteString ()
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Containers ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import Data.GenValidity.URI ()
import ICal.PropertyType

instance GenValid CalAddress where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
