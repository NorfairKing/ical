{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.PropertyType.Duration.Gen where

import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Text ()
import Data.GenValidity.Time ()
import ICal.PropertyType.Duration
import Test.QuickCheck

instance GenValid Duration where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DurDate where
  genValid =
    DurDate
      <$> genValid
      <*> genSized
      <*> genSized
      <*> genSized
      <*> genSized
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DurTime where
  genValid =
    DurTime
      <$> genValid
      <*> genSized
      <*> genSized
      <*> genSized
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid DurWeek where
  genValid =
    DurWeek
      <$> genValid
      <*> genSized
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid Sign where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genSized :: Gen Word
genSized = sized $ \s -> choose (0, fromIntegral s)
