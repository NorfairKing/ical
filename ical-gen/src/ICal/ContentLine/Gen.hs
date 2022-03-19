{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ICal.ContentLine.Gen where

import Data.CaseInsensitive as CI
import Data.GenValidity
import Data.GenValidity.Map ()
import Data.GenValidity.Text
import Data.Text
import qualified Data.Text as T
import ICal.ContentLine
import Test.QuickCheck

instance (FoldCase a, GenValid a) => GenValid (CI a) where
  genValid = CI.mk <$> genValid
  shrinkValid = fmap CI.mk . shrinkValid . CI.original

instance GenValid ContentLine

instance GenValid ContentLineName where
  genValid =
    oneof
      [ ContentLineNameIANA . CI.mk <$> genTextBy genNameChar,
        ContentLineNameX <$> genValid <*> (CI.mk <$> genNonEmptyTextBy genNameChar)
      ]

instance GenValid ParamName where
  genValid =
    oneof
      [ ParamNameIANA . CI.mk <$> genTextBy genNameChar,
        ParamNameX <$> genValid <*> (CI.mk <$> genNonEmptyTextBy genNameChar)
      ]

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

genQSafeChar :: Gen Char
genQSafeChar = genValid `suchThat` (validationIsValid . validateQSafeChar)

genSafeChar :: Gen Char
genSafeChar = genValid `suchThat` (validationIsValid . validateSafeChar)

instance GenValid VendorId where
  genValid = VendorId . CI.mk . T.pack <$> genAtLeastNOf 3 genVendorIdChar

genAtLeastNOf :: Int -> Gen a -> Gen [a]
genAtLeastNOf i g
  | i <= 0 = pure []
  | otherwise = (:) <$> g <*> genAtLeastNOf (pred i) g

genVendorIdChar :: Gen Char
genVendorIdChar = genValid `suchThat` (validationIsValid . validateVendorIdChar)
