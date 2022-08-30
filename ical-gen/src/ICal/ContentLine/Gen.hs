{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.ContentLine.Gen where

import Data.CaseInsensitive as CI
import Data.GenValidity
import Data.GenValidity.CaseInsensitive ()
import Data.GenValidity.Map ()
import Data.GenValidity.Text
import Data.Text
import qualified Data.Text as T
import ICal.ContentLine
import Test.QuickCheck

instance GenValid ContentLine where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ContentLineValue where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance GenValid ContentLineName where
  genValid =
    oneof
      [ ContentLineNameIANA . CI.mk <$> genNonEmptyTextBy genNameChar,
        ContentLineNameX <$> genValid <*> (CI.mk <$> genNonEmptyTextBy genNameChar)
      ]
  shrinkValid = shrinkValidStructurally

instance GenValid ParamName where
  genValid =
    oneof
      [ ParamNameIANA . CI.mk <$> genNonEmptyTextBy genNameChar,
        ParamNameX <$> genValid <*> (CI.mk <$> genNonEmptyTextBy genNameChar)
      ]
  shrinkValid = shrinkValidStructurally

genNonEmptyTextBy :: Gen Char -> Gen Text
genNonEmptyTextBy gen = genTextBy gen `suchThat` (not . T.null)

genNameChar :: Gen Char
genNameChar = genValid `suchThat` (validationIsValid . validateNameChar)

instance GenValid ParamValue where
  genValid =
    oneof
      [ UnquotedParam . CI.mk <$> genTextBy genSafeChar,
        QuotedParam <$> genTextBy genQSafeChar
      ]
  shrinkValid = error "ParamValue"

genQSafeChar :: Gen Char
genQSafeChar = genValid `suchThat` (validationIsValid . validateQSafeChar)

genSafeChar :: Gen Char
genSafeChar = genValid `suchThat` (validationIsValid . validateSafeChar)

instance GenValid VendorId where
  genValid = VendorId . CI.mk . T.pack <$> genAtLeastNOf 3 genVendorIdChar
  shrinkValid = error "VendorId"

genAtLeastNOf :: Int -> Gen a -> Gen [a]
genAtLeastNOf i g
  | i <= 0 = pure []
  | otherwise = (:) <$> g <*> genAtLeastNOf (pred i) g

genVendorIdChar :: Gen Char
genVendorIdChar = genValid `suchThat` (validationIsValid . validateVendorIdChar)
